#pragma once

#include <ostream>
#include <map>
#include <string>
#include "lir.hpp"

class Codegen {
public:
    explicit Codegen(std::ostream& out)
        : out_(out) {}

    void emit_program(const LIR::Program& prog);

private:
    std::ostream& out_;

    // Per-function frame info
    std::map<std::string, long> var_offset_; // VarId -> negative offset from %rbp
    long frame_size_ = 0;                    // total bytes subtracted from %rsp
    long first_root_offset_ = 0;             // e.g. -16(%rbp)
    long num_root_words_ = 0;                // words passed to _cflat_zero_words
    std::map<LIR::StructId, std::map<LIR::FieldId, long>> struct_field_offsets_;
    long gc_root_count_ = 0;                 // number of pointer-typed locals/params for GC

    
    // High-level emission
    void emit_data(const LIR::Program& prog);
    void emit_text(const LIR::Program& prog);
    void emit_function(const LIR::Function& fn, const std::string& name,
                       bool is_main);

    // Frame / locals
    void assign_stack_slots(const LIR::Function& fn);
    void emit_prologue(const LIR::Function& fn, const std::string& name,
                       bool is_main);
    void emit_epilogue(const std::string& name);

    // Helpers for BBs / instructions
    void emit_basic_block(const LIR::Function& fn,
                          const LIR::BasicBlock& bb,
                          const std::string& fn_name);
    void emit_inst(const LIR::Inst& inst, const std::string& fn_name);
    void emit_terminal(const LIR::Terminal& term,
                       const std::string& fn_name);

    // Low-level helpers
    std::string asm_label(const std::string& fn_name,
                          const std::string& bb_label) const;
    std::string slot(const std::string& var) const; // e.g., "-24(%rbp)"

    void emit_const(const LIR::Const& c);
    void emit_copy(const LIR::Copy& c);
    void emit_arith(const LIR::Arith& a);
    void emit_cmp(const LIR::Cmp& c);
    void emit_load(const LIR::Load& l);
    void emit_store(const LIR::Store& s);
    void emit_call(const LIR::Call& call);
    void emit_gfp(const LIR::Gfp& gfp);
    void emit_gep(const LIR::Gep& gep);
    void emit_alloc_single(const LIR::AllocSingle& a);
    void emit_alloc_array(const LIR::AllocArray& a);

    const char* cond_suffix(LIR::RelOp op) const;
};
