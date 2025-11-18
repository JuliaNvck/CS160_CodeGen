#include "codegen.hpp"
#include <algorithm>
#include <vector>
#include <cassert>

using namespace LIR;

void Codegen::emit_program(const Program& prog) {
    emit_data(prog);
    emit_text(prog);
}

void Codegen::emit_data(const Program& prog) {
    out_ << ".data\n\n";

    // Funptr table: for each internal function in prog.funptrs,
    // emit: gfp.<name>: .quad "<name>"
    for (const auto& [fname, ftype] : prog.funptrs) {
        out_ << ".globl gfp." << fname << "\n";
        out_ << "gfp." << fname << ": .quad \"" << fname << "\"\n\n";
    }

    out_ << ".globl __NULL\n";
    out_ << "__NULL: .zero 8\n\n";

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
}

void Codegen::assign_stack_slots(const Function& fn) {
    var_offset_.clear();
    frame_size_ = 0;
    num_root_words_ = 0;
    first_root_offset_ = 0;

    // Put a GC header at -8(%rbp) and locals below that
    // Collect locals into a deterministic order
    std::vector<std::string> vars;
    vars.reserve(fn.locals.size());
    for (const auto& [v, _] : fn.locals) {
        vars.push_back(v);
    }
    std::sort(vars.begin(), vars.end());

    long offset = -16; // first local after GC header (-8)

    for (const auto& v : vars) {
        var_offset_[v] = offset;
        offset -= 8;
        num_root_words_ += 1; // treat all locals as GC roots for now
    }

    first_root_offset_ = -16;

    // words = 1 (GC header) + num_root_words
    long words = 1 + num_root_words_;
    if (words % 2 != 0) {
        words += 1; // padding word if odd number of words
    }
    frame_size_ = words * 8;
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

    // GC header at -8(%rbp). For now, just write 0; you can
    // adjust this if your runtime expects something else.
    out_ << "  movq $0, -8(%rbp)\n";

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
    auto it = var_offset_.find(var);
    if (it == var_offset_.end()) {
        // Special globals like __NULL could be handled here, e.g.
        // return var + "(%rip)";
        // For now, assert; you'll extend this when you wire __NULL.
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
        } else {
            // Gfp, Gep, AllocSingle, AllocArray:
            // You will implement these later.
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
    if (c.lhs == c.op) return;
    out_ << "  movq " << slot(c.op) << ", %r8\n";
    out_ << "  movq %r8, " << slot(c.lhs) << "\n";
}

void Codegen::emit_arith(const Arith& a) {
    switch (a.aop) {
        case ArithOp::Add:
            out_ << "  movq " << slot(a.left) << ", %r8\n";
            out_ << "  movq " << slot(a.right) << ", %r10\n";
            out_ << "  addq %r10, %r8\n";
            out_ << "  movq %r8, " << slot(a.lhs) << "\n";
            break;
        case ArithOp::Sub:
            out_ << "  movq " << slot(a.left) << ", %r8\n";
            out_ << "  movq " << slot(a.right) << ", %r10\n";
            out_ << "  subq %r10, %r8\n";
            out_ << "  movq %r8, " << slot(a.lhs) << "\n";
            break;
        case ArithOp::Mul:
            out_ << "  movq " << slot(a.left) << ", %r8\n";
            out_ << "  movq " << slot(a.right) << ", %r10\n";
            out_ << "  imulq %r10, %r8\n";
            out_ << "  movq %r8, " << slot(a.lhs) << "\n";
            break;
        case ArithOp::Div:
            // Very rough: you should handle sign-extension and
            // %rdx, etc, according to your spec.
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
            break;
    }
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
    out_ << "  movq " << slot(c.left) << ", %rax\n";
    out_ << "  movq " << slot(c.right) << ", %r10\n";
    out_ << "  cmpq %r10, %rax\n";
    out_ << "  movq $0, %rax\n";
    out_ << "  set" << cond_suffix(c.rop) << " %al\n";
    out_ << "  movzbq %al, %rax\n";
    out_ << "  movq %rax, " << slot(c.lhs) << "\n";
}

void Codegen::emit_load(const Load& l) {
    out_ << "  movq " << slot(l.src) << ", %r10\n";
    out_ << "  movq 0(%r10), %rax\n";
    out_ << "  movq %rax, " << slot(l.lhs) << "\n";
}

void Codegen::emit_store(const Store& s) {
    out_ << "  movq " << slot(s.op) << ", %rax\n";
    out_ << "  movq " << slot(s.dst) << ", %r10\n";
    out_ << "  movq %rax, 0(%r10)\n";
}

void Codegen::emit_call(const Call& call) {
    // Skeleton for <= 6 int arguments.
    // Args are in call.args
    // You must:
    //   - move up to 6 into %rdi, %rsi, %rdx, %rcx, %r8, %r9
    //   - push extras in reverse order
    //   - handle direct vs indirect callee
    // For now, we assume:
    //   - number of args <= 6
    //   - callee is a direct function name (no funptrs).
    static const char* arg_regs[] = {"%rdi","%rsi","%rdx","%rcx","%r8","%r9"};

    std::size_t n = call.args.size();
    assert(n <= 6 && "emit_call: only handling <= 6 args in starter code");

    for (std::size_t i = 0; i < n; ++i) {
        out_ << "  movq " << slot(call.args[i]) << ", " << arg_regs[i] << "\n";
    }

    // Very naive: assume callee is a function name recorded as a variable
    // You will likely want to special-case function pointers using prog.funptrs.
    out_ << "  call " << call.callee << "\n";

    if (call.lhs.has_value()) {
        out_ << "  movq %rax, " << slot(*call.lhs) << "\n";
    }
}
