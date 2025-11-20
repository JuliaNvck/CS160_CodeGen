#include "codegen.hpp"
#include <algorithm>
#include <vector>
#include <cassert>

using namespace LIR;

// void Codegen::emit_program(const Program& prog) {
//     struct_field_offsets_.clear();

//     for (const auto& [sid, s] : prog.structs) {
//         long offset = 0;
//         for (const auto& [fname, ftype] : s.fields) {
//             struct_field_offsets_[sid][fname] = offset;
//             offset += 8; // assume each field is 8 bytes
//         }
//     }
    
//     emit_data(prog);
//     emit_text(prog);
// }

void Codegen::emit_program(const LIR::Program& prog) {
    struct_field_offsets_.clear();
    struct_bitmaps_.clear();
    funptr_names_.clear();
    extern_names_.clear();

    for (const auto& [sid, s] : prog.structs) {
        long offset = 0;
        std::size_t idx = 0;
        uint64_t bitmap = 0;

        for (const auto& [fname, ftype] : s.fields) {
            struct_field_offsets_[sid][fname] = offset;
            offset += 8;

            if (is_gc_pointer_type(ftype)) {
                bitmap |= (1ull << idx);   // field idx is a pointer
            }
            ++idx;
        }

        struct_bitmaps_[sid] = bitmap;
    }

    for (const auto& [fname, ftype] : prog.funptrs) {
        funptr_names_.insert(fname);
    }

    for (const auto& [fname, fn] : prog.functions) {
        function_names_.insert(fname);
    }

    for (const auto& [ename, ety] : prog.externs) {
        extern_names_.insert(ename);
    }

    emit_data(prog);
    emit_text(prog);
}

void Codegen::emit_data(const Program& prog) {
    out_ << ".data\n\n";

    for (const auto& [fname, ftype] : prog.funptrs) {
        out_ << ".globl gfp." << fname << "\n";
        out_ << "gfp." << fname << ": .quad \"" << fname << "\"\n\n\n";
    }

    out_ << ".globl __NULL\n";
    out_ << "__NULL: .zero 8\n\n\n";

    out_ << "out_of_bounds_msg: .string \"out-of-bounds array access\"\n";
    out_ << "invalid_alloc_msg: .string \"invalid allocation amount\"\n\n";
}

void Codegen::emit_text(const Program& prog) {
    out_ << ".text\n\n";

    for (const auto& [name, fn] : prog.functions) {
        bool is_main = (name == "main");
        emit_function(fn, name, is_main);
        out_ << "\n";
    }

    // Shared panic helpers
    out_ << ".out_of_bounds:\n";
    out_ << "  lea out_of_bounds_msg(%rip), %rdi\n";
    out_ << "  call _cflat_panic\n\n";

    out_ << ".invalid_alloc_length:\n";
    out_ << "  lea invalid_alloc_msg(%rip), %rdi\n";
    out_ << "  call _cflat_panic\n";
    out_ << "        \n";
}

void Codegen::assign_stack_slots(const Function& fn) {
    var_offset_.clear();
    frame_size_        = 0;
    num_root_words_    = 0;
    first_root_offset_ = 0;
    gc_root_count_     = 0;

    // ---- 1. Partition locals into GC roots vs non-roots ----
    std::vector<std::string> root_vars;
    std::vector<std::string> nonroot_vars;

    root_vars.reserve(fn.locals_order.size());
    nonroot_vars.reserve(fn.locals_order.size());

    for (const auto& v : fn.locals_order) {
        auto it = fn.locals.find(v);
        assert(it != fn.locals.end());
        const auto& ty = it->second;

        bool is_ptr =
            dynamic_cast<const PtrType*>(ty.get())   != nullptr ||
            dynamic_cast<const ArrayType*>(ty.get()) != nullptr;
        bool is_inner = (v.rfind("_inner", 0) == 0);

        if (is_ptr && !is_inner) {
            root_vars.push_back(v);
        } else {
            nonroot_vars.push_back(v);
        }
    }

    std::vector<std::string> ordered_locals;
    ordered_locals.reserve(fn.locals_order.size());
    ordered_locals.insert(ordered_locals.end(), root_vars.begin(), root_vars.end());
    ordered_locals.insert(ordered_locals.end(), nonroot_vars.begin(), nonroot_vars.end());

    // ---- 2. Classify parameters ----
    std::size_t num_params     = fn.params.size();
    std::size_t num_reg_params = (num_params < 6) ? num_params : 6;

    std::vector<bool> is_ptr_param(num_params, false);
    std::vector<std::size_t> ptr_stack_params;    // i >= 6, pointer
    std::vector<std::size_t> ptr_reg_params;      // i < 6, pointer
    std::vector<std::size_t> nonptr_reg_params;   // i < 6, non-pointer

    for (std::size_t i = 0; i < num_params; ++i) {
        const auto& pty = fn.params[i].second;
        bool is_ptr =
            dynamic_cast<const PtrType*>(pty.get())   != nullptr ||
            dynamic_cast<const ArrayType*>(pty.get()) != nullptr;
        is_ptr_param[i] = is_ptr;

        if (i < 6) {
            if (is_ptr) ptr_reg_params.push_back(i);
            else        nonptr_reg_params.push_back(i);
        } else {
            if (is_ptr) ptr_stack_params.push_back(i);
        }
    }

    // ---- 3. Assign stack offsets ----
    long offset           = -16;   // first word below GC header
    long min_local_offset = 0;
    bool first_local      = true;

    // 3a. Pointer params passed on stack → GC root slots first
    for (std::size_t i : ptr_stack_params) {
        const std::string& pname = fn.params[i].first;
        var_offset_[pname] = offset;
        offset -= 8;
    }

    // 3b. Pointer params passed in registers → GC root slots next
    for (std::size_t i : ptr_reg_params) {
        const std::string& pname = fn.params[i].first;
        var_offset_[pname] = offset;
        offset -= 8;
    }

    // 3c. All locals
    for (const auto& v : ordered_locals) {
        var_offset_[v] = offset;

        if (first_local) {
            min_local_offset = offset;
            first_local      = false;
        } else if (offset < min_local_offset) {
            min_local_offset = offset;
        }
        offset -= 8;
    }

    // Only locals are zeroed
    num_root_words_ = static_cast<long>(fn.locals.size());

    // 3d. Non-pointer register params after locals
    for (std::size_t i : nonptr_reg_params) {
        const std::string& pname = fn.params[i].first;
        var_offset_[pname] = offset;
        offset -= 8;
    }

    // 3e. Stack params (i >= 6):
    long stack_param_offset = 16;
    for (std::size_t i = 6; i < num_params; ++i) {
        const std::string& pname = fn.params[i].first;
        if (is_ptr_param[i]) {
            stack_param_offset += 8;
            continue;
        } else {
            var_offset_[pname] = stack_param_offset;
            stack_param_offset += 8;
        }
    }

    // ---- 4. Frame size ----
    long num_spilled_reg_params   = static_cast<long>(num_reg_params);
    long num_spilled_stack_ptrs   = static_cast<long>(ptr_stack_params.size());
    long words = 1                // GC header
               + num_root_words_  // locals
               + num_spilled_reg_params
               + num_spilled_stack_ptrs;

    if (words % 2 != 0) {
        ++words;  // keep 16-byte alignment
    }

    frame_size_ = words * 8;
    first_root_offset_ = (num_root_words_ > 0) ? min_local_offset : 0;

    // ---- 5. GC root count in header ----
    long num_pointer_locals  = static_cast<long>(root_vars.size());
    long num_pointer_params  = static_cast<long>(ptr_stack_params.size() + ptr_reg_params.size());
    gc_root_count_ = num_pointer_locals + num_pointer_params;
}


void Codegen::emit_prologue(const Function& fn,
                            const std::string& name,
                            bool is_main) {
    out_ << ".globl " << name << "\n";
    out_ << name << ":\n";
    out_ << "  pushq %rbp\n";
    out_ << "  movq %rsp, %rbp\n";

    if (frame_size_ > 0) {
        out_ << "  subq $" << frame_size_ << ", %rsp\n";
    }

    // GC header at -8(%rbp): #pointer roots
    out_ << "  movq $" << gc_root_count_ << ", -8(%rbp)\n";

    const char* param_regs[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

    std::size_t num_params     = fn.params.size();
    std::size_t num_reg_params = (num_params < 6) ? num_params : 6;

    std::vector<std::size_t> ptr_stack_indices;
    std::vector<std::size_t> ptr_reg_indices;
    std::vector<std::size_t> nonptr_reg_indices;

    for (std::size_t i = 0; i < num_params; ++i) {
        const auto& pty = fn.params[i].second;
        bool is_ptr =
            dynamic_cast<const PtrType*>(pty.get())   != nullptr ||
            dynamic_cast<const ArrayType*>(pty.get()) != nullptr;

        if (i < 6) {
            if (is_ptr) ptr_reg_indices.push_back(i);
            else        nonptr_reg_indices.push_back(i);
        } else {
            if (is_ptr) ptr_stack_indices.push_back(i);
        }
    }

    auto spill_reg_param = [&](std::size_t i) {
        const auto& pname = fn.params[i].first;
        out_ << "  movq " << param_regs[i] << ", " << slot(pname) << "\n";
    };

    // 1) Pointer params on stack: copy from 16(%rbp)+ into their negative slots
    for (std::size_t i : ptr_stack_indices) {
        const auto& pname = fn.params[i].first;
        long src_off = 16 + 8 * static_cast<long>(i - 6);
        out_ << "  movq " << src_off << "(%rbp), %r10\n";
        out_ << "  movq %r10, " << slot(pname) << "\n";
    }

    // 2) Pointer params in registers
    for (std::size_t i : ptr_reg_indices) {
        spill_reg_param(i);
    }

    // 3) Non-pointer params in registers
    for (std::size_t i : nonptr_reg_indices) {
        spill_reg_param(i);
    }

    // Zero locals (not parameter copies)
    if (num_root_words_ > 0) {
        out_ << "  movq %rbp, %rdi\n";
        out_ << "  addq $" << first_root_offset_ << ", %rdi\n";
        out_ << "  movq $" << num_root_words_ << ", %rsi\n";
        out_ << "  call _cflat_zero_words\n";
    }

    if (is_main) {
        out_ << "  call _cflat_init_gc\n";
    }

    out_ << "  jmp " << asm_label(name, "entry") << "\n\n";
}

void Codegen::emit_epilogue(const std::string& name) {
    out_ << name << "_epilogue:\n";
    out_ << "  movq %rbp, %rsp\n";
    out_ << "  popq %rbp\n";
    out_ << "  ret\n";
}

std::string Codegen::asm_label(const std::string& fn_name,
                               const std::string& bb_label) const {
    return fn_name + "_" + bb_label;
}

std::string Codegen::slot(const std::string& var) const {
    // Special global constant
    if (var == "__NULL") {
        return "__NULL(%rip)";
    }

    // Stack locals / params
    auto it = var_offset_.find(var);
    if (it != var_offset_.end()) {
        long off = it->second;
        return std::to_string(off) + "(%rbp)";
    }

    // Global function-pointer variables (keys of prog.funptrs)
    if (funptr_names_.count(var)) {
        return "gfp." + var + "(%rip)";
    }

    std::cerr << "ERROR: Unknown variable '" << var << "' in slot()\n";
    std::cerr << "Available variables in var_offset_: " << var_offset_.size() << "\n";
    assert(false && "Unknown variable in slot()");
    return ""; // unreachable
}

void Codegen::emit_function(const Function& fn,
                            const std::string& name,
                            bool is_main) {
    assign_stack_slots(fn);
    emit_prologue(fn, name, is_main);

    // Emit basic blocks
    for (const auto& [bb_name, bb] : fn.body) {
        out_ << asm_label(name, bb.label) << ":\n";
        emit_basic_block(fn, bb, name);
        out_ << "\n";
    }

    emit_epilogue(name);
}

void Codegen::emit_basic_block(const Function&,
                               const BasicBlock& bb,
                               const std::string& fn_name) {
    for (const auto& inst : bb.insts) {
        emit_inst(inst, fn_name);
    }
    emit_terminal(bb.term, fn_name);
}

void Codegen::emit_inst(const Inst& inst,
                        const std::string&) {
    std::visit([this](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Const>) {
            emit_const(arg);
        } else if constexpr (std::is_same_v<T, Copy>) {
            emit_copy(arg);
        } else if constexpr (std::is_same_v<T, Arith>) {
            emit_arith(arg);
        } else if constexpr (std::is_same_v<T, Cmp>) {
            emit_cmp(arg);
        } else if constexpr (std::is_same_v<T, Load>) {
            emit_load(arg);
        } else if constexpr (std::is_same_v<T, Store>) {
            emit_store(arg);
        } else if constexpr (std::is_same_v<T, Call>) {
            emit_call(arg);
        } else if constexpr (std::is_same_v<T, Gfp>) {
            emit_gfp(arg);
        } else if constexpr (std::is_same_v<T, Gep>) {
            emit_gep(arg);
        } else if constexpr (std::is_same_v<T, AllocSingle>) {
            emit_alloc_single(arg);
        } else if constexpr (std::is_same_v<T, AllocArray>) {
            emit_alloc_array(arg);
        }
    }, inst);
}

void Codegen::emit_terminal(const Terminal& term,
                            const std::string& fn_name) {
    std::visit([this, &fn_name](auto&& arg) {
        using T = std::decay_t<decltype(arg)>; // detect which concrete terminal type arg is
        if constexpr (std::is_same_v<T, Jump>) {
            out_ << "  jmp " << asm_label(fn_name, arg.target) << "\n";
        } else if constexpr (std::is_same_v<T, Branch>) {
            // if guard != 0 → tt else ff
            out_ << "  movq " << slot(arg.guard) << ", %r8\n";
            out_ << "  cmpq $0, %r8\n";
            out_ << "  jne " << asm_label(fn_name, arg.tt) << "\n";
            out_ << "  jmp " << asm_label(fn_name, arg.ff) << "\n";
        } else if constexpr (std::is_same_v<T, Ret>) {
            if (arg.val.has_value()) {
                out_ << "  movq " << slot(*arg.val) << ", %rax\n";
            }
            out_ << "  jmp " << fn_name << "_epilogue\n";
        } else {
        }
    }, term);
}

// ---- per-inst helpers ----

void Codegen::emit_const(const Const& c) {
    out_ << "  movq $" << c.val << ", " << slot(c.lhs) << "\n";
}

void Codegen::emit_copy(const Copy& c) {
    // if (c.lhs == c.op) return;
    out_ << "  movq " << slot(c.op) << ", %r8\n";
    out_ << "  movq %r8, " << slot(c.lhs) << "\n";
}

void Codegen::emit_arith(const Arith& a) {
    if (a.aop == ArithOp::Div) {
        out_ << "  movq " << slot(a.left) << ", %rax\n"; 
        out_ << "  movq " << slot(a.right) << ", %r8\n"; 
        out_ << "  cmpq $0, %r8\n"; 
        out_ << "  movq $1, %r9\n";
        out_ << "  cmoveq %r9, %r8\n";
        out_ << "  movq $0, %r9\n";
        out_ << "  cmoveq %r9, %rax\n";
        out_ << "  cqo\n";
        out_ << "  idivq %r8\n";
        out_ << "  movq %rax, " << slot(a.lhs) << "\n";
        return;
    }
    out_ << "  movq " << slot(a.left) << ", %r8\n";
    switch (a.aop) {
        case ArithOp::Add:
            out_ << "  addq " << slot(a.right) << ", %r8\n";
            break;
        case ArithOp::Sub:
            out_ << "  subq " << slot(a.right) << ", %r8\n";
            break;
        case ArithOp::Mul:
            out_ << "  imulq " << slot(a.right) << ", %r8\n";
            break;
        default:
            break;
    }
    out_ << "  movq %r8, " << slot(a.lhs) << "\n";
}

const char* Codegen::cond_suffix(RelOp op) const {
    switch (op) {
        case RelOp::Eq:    return "e";
        case RelOp::NotEq: return "ne";
        case RelOp::Lt:    return "l";
        case RelOp::Lte:   return "le";
        case RelOp::Gt:    return "g";
        case RelOp::Gte:   return "ge";
    }
    return "e";
}

void Codegen::emit_cmp(const Cmp& c) {
    out_ << "  movq " << slot(c.left) << ", %r8\n"; 
    out_ << "  cmpq " << slot(c.right) << ", %r8\n";
    out_ << "  movq $0, %r8\n"; 
    out_ << "  set" << cond_suffix(c.rop) << " %r8b\n";
    out_ << "  movq %r8, " << slot(c.lhs) << "\n";
}

// void Codegen::emit_load(const Load& l) {
//     out_ << "  movq " << slot(l.src) << ", %r10\n";
//     out_ << "  movq 0(%r10), %rax\n";
//     out_ << "  movq %rax, " << slot(l.lhs) << "\n";
// }

void Codegen::emit_load(const Load& l) {
    out_ << "  movq " << slot(l.src) << ", %r8\n";
    out_ << "  movq 0(%r8), %r9\n";
    out_ << "  movq %r9, " << slot(l.lhs) << "\n";
}


// void Codegen::emit_store(const Store& s) {
//     out_ << "  movq " << slot(s.op) << ", %rax\n";
//     out_ << "  movq " << slot(s.dst) << ", %r10\n";
//     out_ << "  movq %rax, 0(%r10)\n";
// }

void Codegen::emit_store(const Store& s) {
    out_ << "  movq " << slot(s.op) << ", %r8\n";
    out_ << "  movq " << slot(s.dst) << ", %r9\n";
    out_ << "  movq %r8, 0(%r9)\n";
}


void Codegen::emit_call(const Call& call) {
    // Handle calls with any number of arguments:
    //   - move up to 6 into %rdi, %rsi, %rdx, %rcx, %r8, %r9
    //   - push extras in reverse order (arg 6+n, ..., arg 7, arg 6)
    //   - maintain 16-byte stack alignment before call
    static const char* arg_regs[] = {"%rdi","%rsi", "%rdx", "%rcx","%r8","%r9"};

    std::size_t n = call.args.size();
    std::size_t num_stack_args = (n > 6) ? (n - 6) : 0;
    
    // Calculate alignment padding needed
    // After pushing num_stack_args, %rsp must be 16-byte aligned
    // If num_stack_args is odd, we need 8 bytes of padding
    long alignment_padding = (num_stack_args % 2 == 1) ? 8 : 0;
    
    // Add alignment padding if needed
    if (alignment_padding > 0) {
        out_ << "  subq $" << alignment_padding << ", %rsp\n";
    }
    
    // Load first 6 args into registers
    std::size_t num_reg_args = (n < 6) ? n : 6;
    for (std::size_t i = 0; i < num_reg_args; ++i) {
        out_ << "  movq " << slot(call.args[i]) << ", " << arg_regs[i] << "\n";
    }
    
    // Push remaining args onto stack in reverse order
    if (num_stack_args > 0) {
        for (std::size_t i = n - 1; i >= 6; --i) {
            out_ << "  pushq " << slot(call.args[i]) << "\n";
            if (i == 6) break; // prevent underflow
        }
    }

    bool is_direct =
        function_names_.count(call.callee) > 0 ||
        extern_names_.count(call.callee)   > 0;

    if (is_direct) {
        // call known function / extern by name
        out_ << "  call " << call.callee << "\n";
    } else {
        // call through function pointer variable
        out_ << "  movq " << slot(call.callee) << ", %r10\n";
        out_ << "  call *%r10\n";
    }
    
    // Store return value if needed
    if (call.lhs.has_value()) {
        out_ << "  movq %rax, " << slot(*call.lhs) << "\n";
    }
    
    // Clean up stack args and alignment padding
    long total_cleanup = num_stack_args * 8 + alignment_padding;
    if (total_cleanup > 0) {
        out_ << "  addq $" << total_cleanup << ", %rsp\n";
    }
}

// void Codegen::emit_gfp(const Gfp& g) {
//     // src is a pointer to the struct
//     out_ << "  movq " << slot(g.src) << ", %r8\n";

//     long off = 0;
//     auto sit = struct_field_offsets_.find(g.sid);
//     if (sit != struct_field_offsets_.end()) {
//         auto fit = sit->second.find(g.field);
//         if (fit != sit->second.end()) {
//             off = fit->second;
//         }
//     }
//     // If we didn't find it, we just treat offset as 0 to avoid crashing;
//     // but ideally this never happens.

//     if (off != 0) {
//         out_ << "  addq $" << off << ", %r8\n";
//     }
//     out_ << "  movq %r8, " << slot(g.lhs) << "\n";
// }

void Codegen::emit_gfp(const Gfp& g) {
    out_ << "  movq " << slot(g.src) << ", %r8\n";
    long off = struct_field_offsets_.at(g.sid).at(g.field);
    out_ << "  leaq " << off << "(%r8), %r9\n";
    out_ << "  movq %r9, " << slot(g.lhs) << "\n";
}

void Codegen::emit_gep(const Gep& g) {
    // idx -> %r8
    out_ << "  movq " << slot(g.idx) << ", %r8\n";

    if (g.checked) {
        // Bounds check:
        // if idx < 0 → .out_of_bounds
        out_ << "  cmpq $0, %r8\n";
        out_ << "  jl .out_of_bounds\n";

        // array ptr -> %r9
        out_ << "  movq " << slot(g.src) << ", %r9\n";
        // header word at -8(%r9) (encoded: len*8 + tag)
        out_ << "  movq -8(%r9), %r10\n";
        // len = header >> 3
        out_ << "  shrq $3, %r10\n";
        // if idx >= len → .out_of_bounds
        out_ << "  cmpq %r10, %r8\n";
        out_ << "  jge .out_of_bounds\n";
    } else {
        // still need the array pointer in %r9 for address calc
        out_ << "  movq " << slot(g.src) << ", %r9\n";
    }

    // Compute address: p + idx * 8
    out_ << "  imulq $8, %r8\n";
    out_ << "  addq %r9, %r8\n";
    out_ << "  movq %r8, " << slot(g.lhs) << "\n";
}

// void Codegen::emit_alloc_single(const AllocSingle& a) {
//     // Very simple stub: allocate 2 words (header + 1 payload word)
//     // rdi = #words
//     out_ << "  movq $2, %rdi\n";
//     out_ << "  call _cflat_alloc\n";

//     // header: 1 element word => len = 1 => header = 1*8 + 2 = 10
//     // (You can tune this if your runtime expects a different tag.)
//     out_ << "  movq $10, %r8\n";
//     out_ << "  movq %r8, 0(%rax)\n";

//     // result pointer = %rax + 8
//     out_ << "  addq $8, %rax\n";
//     out_ << "  movq %rax, " << slot(a.lhs) << "\n";
// }
void Codegen::emit_alloc_single(const LIR::AllocSingle& a) {
    LIR::TypePtr t = a.typ;

    // CASE 1: boxed Int
    if (dynamic_cast<const LIR::IntType*>(t.get())) {
        out_ << "  movq $2, %rdi\n";
        out_ << "  call _cflat_alloc\n";
        out_ << "  movabsq $4, %r8\n";
        out_ << "  movq %r8, 0(%rax)\n";
        out_ << "  addq $8, %rax\n";
        out_ << "  movq %rax, " << slot(a.lhs) << "\n";
        return;
    }

    // CASE 2: struct
    if (auto st = dynamic_cast<const StructType*>(t.get())) {
        const std::string& sid = st->id;

        // number of fields = number of offsets we stored
        std::size_t nfields = struct_field_offsets_.at(sid).size();

        // bitmap from our precomputed map
        uint64_t bitmap = struct_bitmaps_.at(sid);

        // tag = 0 for non-array objects
        //uint64_t header = bitmap << 3;
        //uint64_t header = 260ull * bitmap;
        uint64_t header = (bitmap << 8) | 8;

        // allocate header + fields = nfields + 1
        out_ << "  movq $" << (nfields + 1) << ", %rdi\n";
        out_ << "  call _cflat_alloc\n";

        // store header
        out_ << "  movabsq $" << header << ", %r8\n";
        out_ << "  movq %r8, 0(%rax)\n";

        // compute address of field 0
        out_ << "  addq $8, %rax\n";

        // store result
        out_ << "  movq %rax, " << slot(a.lhs) << "\n";
        return;
    }

    if (dynamic_cast<const LIR::PtrType*>(t.get()) ||
        dynamic_cast<const LIR::ArrayType*>(t.get())) {
        out_ << "  movq $2, %rdi\n";
        out_ << "  call _cflat_alloc\n";
        // bitmap = 1 (payload word is a pointer), tag = 4
        out_ << "  movabsq $260, %r8\n";   // (1 << 8) | 4
        out_ << "  movq %r8, 0(%rax)\n";
        out_ << "  addq $8, %rax\n";
        out_ << "  movq %rax, " << slot(a.lhs) << "\n";
        return;
    }
}


void Codegen::emit_alloc_array(const AllocArray& a) {
    // length <= 0 → invalid_alloc_length
    out_ << "  cmpq $0, " << slot(a.amt) << "\n";
    out_ << "  jle .invalid_alloc_length\n";

    // length >= 2^61 → invalid_alloc_length (avoid overflow)
    out_ << "  movq $1, %r8\n";
    out_ << "  shlq $61, %r8\n";
    out_ << "  cmpq %r8, " << slot(a.amt) << "\n";
    out_ << "  jge .invalid_alloc_length\n";

    // rdi = length + 1  (#words: 1 header + length elements)
    out_ << "  movq " << slot(a.amt) << ", %rdi\n";
    out_ << "  addq $1, %rdi\n";
    out_ << "  call _cflat_alloc\n";

    // compute header word: header = len*8 + tag
    int tag = 2;  // default: array of non-GC values (e.g., Int)

    const TypePtr &elemTy = a.typ;
    bool elem_is_ptr =
        dynamic_cast<const PtrType*>(elemTy.get())   != nullptr ||
        dynamic_cast<const ArrayType*>(elemTy.get()) != nullptr;

    if (elem_is_ptr) {
        tag = 6;   // array of GC pointers
    }
    
    
    out_ << "  movq " << slot(a.amt) << ", %r8\n";
    out_ << "  shlq $3, %r8\n";
    out_ << "  addq $" << tag << ", %r8\n";
    out_ << "  movq %r8, 0(%rax)\n";

    // result pointer = &data[0] = %rax + 8
    out_ << "  addq $8, %rax\n";
    out_ << "  movq %rax, " << slot(a.lhs) << "\n";
}

bool Codegen::is_gc_pointer_type(const LIR::TypePtr& ty) const {
    return dynamic_cast<const LIR::PtrType*>(ty.get())   != nullptr ||
           dynamic_cast<const LIR::ArrayType*>(ty.get()) != nullptr;
}
