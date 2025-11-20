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

    emit_data(prog);
    emit_text(prog);
}

void Codegen::emit_data(const Program& prog) {
    out_ << ".data\n\n";

    // Funptr table: for each internal function in prog.funptrs,
    // emit: gfp.<name>: .quad "<name>"
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
    frame_size_ = 0;
    num_root_words_ = 0;
    first_root_offset_ = 0;
    gc_root_count_ = 0;

    //
    // 1. Partition locals into GC roots vs non-roots, preserving locals_order
    //
    std::vector<std::string> root_vars;     // non-inner pointer/array locals
    std::vector<std::string> nonroot_vars; // everything else

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
            // GC-root local
            root_vars.push_back(v);
        } else {
            // non-root local (ints, _const_*, _inner*, etc.)
            nonroot_vars.push_back(v);
        }
    }

    std::vector<std::string> ordered_vars;
    ordered_vars.reserve(fn.locals_order.size());
    // All GC-root locals first, then all non-root locals
    ordered_vars.insert(ordered_vars.end(), root_vars.begin(), root_vars.end());
    ordered_vars.insert(ordered_vars.end(), nonroot_vars.begin(), nonroot_vars.end());

    //
    // 2. Assign stack offsets for locals
    //
    long offset = -16;          // first local after GC header at -8(%rbp)
    long min_local_offset = 0;  // most negative local offset
    bool first_local = true;

    for (const auto& v : ordered_vars) {
        var_offset_[v] = offset;

        if (first_local) {
            min_local_offset = offset;
            first_local = false;
        } else if (offset < min_local_offset) {
            min_local_offset = offset;
        }

        offset -= 8; // next local
    }

    // All locals are zeroed by _cflat_zero_words
    num_root_words_ = static_cast<long>(fn.locals.size());

    //
    // 3. Parameters (same as before)
    //
    std::size_t num_params     = fn.params.size();
    std::size_t num_reg_params = (num_params < 6) ? num_params : 6;
    long num_pointer_params    = 0;

    // First up to 6 params: passed in registers → spill to negative offsets
    for (std::size_t i = 0; i < num_reg_params; ++i) {
        const auto& [pname, ptype] = fn.params[i];
        var_offset_[pname] = offset;

        bool is_ptr =
            dynamic_cast<const PtrType*>(ptype.get())   != nullptr ||
            dynamic_cast<const ArrayType*>(ptype.get()) != nullptr;
        if (is_ptr) {
            ++num_pointer_params;
        }

        offset -= 8;
    }

    // Params 7+ already on stack: 16(%rbp), 24(%rbp), ...
    long stack_param_offset = 16;
    for (std::size_t i = 6; i < num_params; ++i) {
        const auto& [pname, ptype] = fn.params[i];
        var_offset_[pname] = stack_param_offset;
        stack_param_offset += 8;
    }

    //
    // 4. Frame size & zeroing region
    //
    long words = 1                // GC header word
               + num_root_words_  // locals
               + static_cast<long>(num_reg_params); // spilled reg params

    if (words % 2 != 0) {
        ++words;                  // keep 16-byte alignment
    }

    frame_size_ = words * 8;
    first_root_offset_ = (num_root_words_ > 0) ? min_local_offset : 0;

    //
    // 5. GC root count in header = root locals + pointer params
    //
    long num_pointer_locals = static_cast<long>(root_vars.size());
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

    // GC header at -8(%rbp): store the count of pointer-typed locals/params
    out_ << "  movq $" << gc_root_count_ << ", -8(%rbp)\n";

    // Save register-passed parameters to stack
    // First 6 parameters are passed in: %rdi, %rsi, %rdx, %rcx, %r8, %r9
    const char* param_regs[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
    for (size_t i = 0; i < fn.params.size() && i < 6; ++i) {
        const auto& [pname, ptype] = fn.params[i];
        out_ << "  movq " << param_regs[i] << ", " << slot(pname) << "\n";
    }

    // Zero root words if any
    if (num_root_words_ > 0) {
        out_ << "  movq %rbp, %rdi\n";
        out_ << "  addq $" << first_root_offset_ << ", %rdi\n";
        out_ << "  movq $" << num_root_words_ << ", %rsi\n";
        out_ << "  call _cflat_zero_words\n";
    }

    if (is_main) {
        out_ << "  call _cflat_init_gc\n";
    }

    // Jump to entry block
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
    // Handle special global variables
    if (var == "__NULL") {
        return "__NULL(%rip)";
    }
    
    auto it = var_offset_.find(var);
    if (it == var_offset_.end()) {
        // Unknown variable
        std::cerr << "ERROR: Unknown variable '" << var << "' in slot()\n";
        std::cerr << "Available variables in var_offset_: " << var_offset_.size() << "\n";
        assert(false && "Unknown variable in slot()");
    }
    long off = it->second;
    return std::to_string(off) + "(%rbp)";
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
                        const std::string& /*fn_name*/) {
    std::visit([this](auto&& arg) {
        using T = std::decay_t<decltype(arg)>; // detect which concrete instruction type arg is
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
            // std::monostate: no terminal (shouldn't happen)
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
        out_ << "  movq " << slot(a.left) << ", %rax\n"; // Load the dividend into %rax
        out_ << "  movq " << slot(a.right) << ", %r8\n"; // Load the divisor into %r8
        out_ << "  cmpq $0, %r8\n"; // Compare divisor with 0
        out_ << "  movq $1, %r9\n";
        out_ << "  cmoveq %r9, %r8\n"; // If previous comparison was equal (%r8 == 0), move 1 into %r8 to avoid division by zero
        out_ << "  movq $0, %r9\n";
        out_ << "  cmoveq %r9, %rax\n"; // If divisor was 0, then copy 0 into %rax
        out_ << "  cqo\n"; // sign-extend %rax into %rdx, treats %rax as a 64-bit signed integer and fills %rdx with its sign bit   
        out_ << "  idivq %r8\n"; // signed division, quotient in %rax, %rdx = remainder
        out_ << "  movq %rax, " << slot(a.lhs) << "\n";
        return;
    }
    // non-division arithmetic: use %r8 and memory operand for rhs
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
    out_ << "  movq " << slot(c.left) << ", %r8\n"; // lhs in %r8
    out_ << "  cmpq " << slot(c.right) << ", %r8\n"; // compare rhs (memory) against %r8
    out_ << "  movq $0, %r8\n"; // zero full %r8
    out_ << "  set" << cond_suffix(c.rop) << " %r8b\n"; // set condition into low byte of %r8
    out_ << "  movq %r8, " << slot(c.lhs) << "\n"; // store boolean (0/1) into dest
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
    static const char* arg_regs[] = {"%rdi","%rsi","%rdx","%rcx","%r8","%r9"};

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

    // Call the function
    out_ << "  call " << call.callee << "\n";
    
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
    //long off = /* 8 * field_index, from struct_field_offsets_ */;
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
