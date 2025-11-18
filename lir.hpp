#pragma once

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <list>
#include <variant>
#include <memory>
#include <optional>
#include <iomanip>

namespace LIR {

// --- LIR Type System ---
struct Type;
using TypePtr = std::shared_ptr<Type>;

// Forward declare LIR-specific compound types used below
struct ArrayType;
struct PtrType;

struct Type {
    virtual ~Type() = default;
    virtual void print(std::ostream& os) const = 0;
    virtual bool equals(const Type& other) const = 0;
};

inline std::ostream& operator<<(std::ostream& os, const Type& type) {
    type.print(os);
    return os;
}
inline std::ostream& operator<<(std::ostream& os, const TypePtr& type) {
    if (type) type->print(os); else os << "<null_type>";
    return os;
}

struct IntType : Type {
    void print(std::ostream& os) const override { os << "int"; }
    bool equals(const Type& other) const override { return dynamic_cast<const IntType*>(&other); }
};

struct NilType : Type {
    void print(std::ostream& os) const override { os << "nil"; }
    bool equals(const Type& other) const override;
};

struct StructType : Type {
    std::string id;
    explicit StructType(std::string i) : id(std::move(i)) {}
    void print(std::ostream& os) const override { os << id; }
    bool equals(const Type& other) const override {
        auto o = dynamic_cast<const StructType*>(&other);
        return o && o->id == id;
    }
};

struct ArrayType : Type {
    TypePtr element;
    explicit ArrayType(TypePtr e) : element(std::move(e)) {}
    void print(std::ostream& os) const override { os << "[" << element << "]"; }
    bool equals(const Type& other) const override {
        if (dynamic_cast<const NilType*>(&other)) return true;
        auto o = dynamic_cast<const ArrayType*>(&other);
        return o && element->equals(*o->element);
    }
};

// NilType::equals will be defined after PtrType is complete

struct PtrType : Type {
    TypePtr element;
    explicit PtrType(TypePtr e) : element(std::move(e)) {}
    void print(std::ostream& os) const override; // Defined after FnType
    bool equals(const Type& other) const override {
        if (dynamic_cast<const NilType*>(&other)) return true;
        auto o = dynamic_cast<const PtrType*>(&other);
        return o && element->equals(*o->element);
    }
};

// Now that PtrType (and ArrayType) are complete, define NilType::equals
inline bool NilType::equals(const Type& other) const {
    return dynamic_cast<const NilType*>(&other) ||
           dynamic_cast<const PtrType*>(&other) ||
           dynamic_cast<const ArrayType*>(&other);
}

struct FnType : Type {
    std::vector<TypePtr> params;
    TypePtr ret;
    FnType(std::vector<TypePtr> p, TypePtr r) : params(std::move(p)), ret(std::move(r)) {}
    void print(std::ostream& os) const override {
        os << "fn (";
        for (size_t i = 0; i < params.size(); ++i) {
            os << params[i] << (i == params.size() - 1 ? "" : ", ");
        }
        os << ") -> " << ret;
    }
    bool equals(const Type& other) const override {
        auto o = dynamic_cast<const FnType*>(&other);
        if (!o || params.size() != o->params.size() || !ret->equals(*o->ret)) {
            return false;
        }
        for (size_t i = 0; i < params.size(); ++i) {
            if (!params[i]->equals(*o->params[i])) return false;
        }
        return true;
    }
};

// Now that FnType is complete, define PtrType::print
inline void PtrType::print(std::ostream& os) const {
    // Special case: function pointers print as &(...) -> ret, not &fn (...) -> ret
    if (auto fn = dynamic_cast<const FnType*>(element.get())) {
        os << "&(";
        for (size_t i = 0; i < fn->params.size(); ++i) {
            os << fn->params[i] << (i == fn->params.size() - 1 ? "" : ", ");
        }
        os << ") -> " << fn->ret;
    } else {
        os << "&" << element;
    }
}

// --- LIR Data Structures ---

using VarId = std::string;
using BbId = std::string;
using FuncId = std::string;
using StructId = std::string;
using FieldId = std::string;

// Operands
enum class ArithOp { Add, Sub, Mul, Div };
enum class RelOp { Eq, NotEq, Lt, Lte, Gt, Gte };

// --- Instructions (Inst) ---
struct Const { VarId lhs; int val; };
struct Copy { VarId lhs; VarId op; };
struct Arith { VarId lhs; ArithOp aop; VarId left; VarId right; };
struct Cmp { VarId lhs; RelOp rop; VarId left; VarId right; };
struct Load { VarId lhs; VarId src; };
struct Store { VarId dst; VarId op; };
struct Gfp { VarId lhs; VarId src; StructId sid; FieldId field; }; // GetFieldPtr
struct Gep { VarId lhs; VarId src; VarId idx; bool checked; };     // GetElementPtr
struct AllocSingle { VarId lhs; TypePtr typ; }; // Changed to use LIR::Type
struct AllocArray { VarId lhs; VarId amt; TypePtr typ; }; // Changed to use LIR::Type
struct Call { std::optional<VarId> lhs; VarId callee; std::vector<VarId> args; };

using Inst = std::variant<
    Const, Copy, Arith, Cmp, Load, Store, Gfp, Gep, AllocSingle, AllocArray, Call
>;

// --- Terminals ---
struct Jump { BbId target; };
struct Branch { VarId guard; BbId tt; BbId ff; };
struct Ret { std::optional<VarId> val; }; // Using optional for return; vs return x;

using Terminal = std::variant<std::monostate, Jump, Branch, Ret>;

// --- Core Structures ---
struct BasicBlock {
    BbId label;
    std::vector<Inst> insts;
    Terminal term;
};

struct Function {
    FuncId name;
    std::vector<std::pair<VarId, TypePtr>> params;
    TypePtr rettyp;
    std::map<VarId, TypePtr> locals;
    std::map<BbId, BasicBlock> body;
};

struct Extern {
    FuncId name;
    TypePtr typ; // This will be a Ptr(Fn(...))
};

struct Struct {
    StructId name;
    std::map<FieldId, TypePtr> fields;
};

struct Program {
    std::map<StructId, Struct> structs;
    std::map<FuncId, TypePtr> externs; // Name -> FnType
    std::map<FuncId, TypePtr> funptrs; // Name -> Ptr(FnType)
    std::map<FuncId, Function> functions;

    friend std::ostream& operator<<(std::ostream& os, const Program& prog);
};

// --- LIR Printers ---

inline std::ostream& operator<<(std::ostream& os, ArithOp op) {
    switch (op) {
        case ArithOp::Add: return os << "add";
        case ArithOp::Sub: return os << "sub";
        case ArithOp::Mul: return os << "mul";
        case ArithOp::Div: return os << "div";
    }
    return os;
}

inline std::ostream& operator<<(std::ostream& os, RelOp op) {
    switch (op) {
        case RelOp::Eq:    return os << "eq";
        case RelOp::NotEq: return os << "ne";
        case RelOp::Lt:    return os << "lt";
        case RelOp::Lte:   return os << "lte";
        case RelOp::Gt:    return os << "gt";
        case RelOp::Gte:   return os << "gte";
    }
    return os;
}

inline std::ostream& operator<<(std::ostream& os, const Inst& inst) {
    os << "  "; // Indentation
    std::visit([&os](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Const>)
            os << arg.lhs << " = $const " << arg.val;
        else if constexpr (std::is_same_v<T, Copy>)
            os << arg.lhs << " = $copy " << arg.op;
        else if constexpr (std::is_same_v<T, Arith>)
            os << arg.lhs << " = $arith " << arg.aop << " " << arg.left << " " << arg.right;
        else if constexpr (std::is_same_v<T, Cmp>)
            os << arg.lhs << " = $cmp " << arg.rop << " " << arg.left << " " << arg.right;
        else if constexpr (std::is_same_v<T, Load>)
            os << arg.lhs << " = $load " << arg.src;
        else if constexpr (std::is_same_v<T, Store>)
            os << "$store " << arg.dst << " " << arg.op;
        else if constexpr (std::is_same_v<T, Gfp>)
            os << arg.lhs << " = $gfp " << arg.src << " " << arg.sid << "::" << arg.field;
        else if constexpr (std::is_same_v<T, Gep>)
            os << arg.lhs << " = $gep " << arg.src << " " << arg.idx << " [" << (arg.checked ? "true" : "false") << "]";
        else if constexpr (std::is_same_v<T, AllocSingle>)
             os << arg.lhs << " = $alloc_single " << arg.typ;
        else if constexpr (std::is_same_v<T, AllocArray>)
             os << arg.lhs << " = $alloc_array " << arg.amt << " " << arg.typ;
        else if constexpr (std::is_same_v<T, Call>) {
            if (arg.lhs) os << *arg.lhs << " = ";
            os << "$call " << arg.callee << "(";
            // Args are stored in reverse order, so print them backwards
            for(int i = arg.args.size() - 1; i >= 0; --i) {
                os << arg.args[i];
                if (i > 0) os << ", ";
            }
            os << ")";
        }
    }, inst);
    return os << "\n";
}

// inline std::ostream& operator<<(std::ostream& os, const Terminal& term) {
//     os << "  "; // Indentation
//     std::visit([&os](auto&& arg) {
//         using T = std::decay_t<decltype(arg)>;
//         if constexpr (std::is_same_v<T, Jump>)
//             os << "$jump " << arg.target;
//         else if constexpr (std::is_same_v<T, Branch>)
//             os << "$branch " << arg.guard << " " << arg.tt << " " << arg.ff;
//         else if constexpr (std::is_same_v<T, Ret>) {
//             os << "$ret";
//             if (arg.val) os << " " << *arg.val;
//         }
//         else if constexpr (std::is_same_v<T, std::monostate>)
//             os << "$unreachable"; // Should not happen in valid Cflat
//     }, term);
//     return os << "\n";
// }

// inline std::ostream& operator<<(std::ostream& os, const Program& prog) {
//     // Print function pointers (lexicographically)
//     for (const auto& [name, type] : prog.funptrs) {
//         os << "funptr " << name << ": " << type << "\n";
//     }
//     if (!prog.funptrs.empty()) os << "\n";

//     // Print structs (lexicographically)
//     for (const auto& [name, s] : prog.structs) {
//         os << "struct " << name << " {\n";
//         for (const auto& [fname, ftype] : s.fields) {
//             os << "  " << fname << ": " << ftype << "\n";
//         }
//         os << "}\n\n";
//     }

//     // Print externs (lexicographically)
//     for (const auto& [name, type] : prog.externs) {
//         os << "extern " << name << ": ";
//         // Externs have FnType but print without "fn" keyword
//         if (auto fn = dynamic_cast<const FnType*>(type.get())) {
//             os << "(";
//             for (size_t i = 0; i < fn->params.size(); ++i) {
//                 os << fn->params[i] << (i == fn->params.size() - 1 ? "" : ", ");
//             }
//             os << ") -> " << fn->ret;
//         } else {
//             os << type;
//         }
//         os << "\n";
//     }
//     if (!prog.externs.empty()) os << "\n";

//     // Print functions (lexicographically)
//     for (const auto& [name, func] : prog.functions) {
//         os << "fn " << name << "(";
//         for (size_t i = 0; i < func.params.size(); ++i) {
//             os << func.params[i].first << ":" << func.params[i].second;
//             if (i < func.params.size() - 1) os << ", ";
//         }
//         os << ") -> " << func.rettyp << " {\n";

//         // Print locals (lexicographically), excluding params
//         // First, create a set of param names for quick lookup
//         std::set<std::string> param_names;
//         for (const auto& [pname, ptype] : func.params) {
//             param_names.insert(pname);
//         }
        
//         // Count non-param locals
//         std::map<VarId, TypePtr> non_param_locals;
//         for (const auto& [local, type] : func.locals) {
//             if (param_names.find(local) == param_names.end()) {
//                 non_param_locals[local] = type;
//             }
//         }
        
//         if (!non_param_locals.empty()) {
//             os << "let ";
//             size_t i = 0;
//             for (const auto& [local, type] : non_param_locals) {
//                 os << local << ":" << type;
//                 if (i < non_param_locals.size() - 1) os << ", ";
//                 i++;
//             }
//             os << "\n";
//         }

//         // Print basic blocks (entry first, then lexicographical)
//         std::list<BbId> labels;
//         std::string entry_label = "entry";
//         if (func.body.count(entry_label)) {
//             labels.push_back(entry_label);
//         }
//         for (const auto& [label, bb] : func.body) {
//             if (label != entry_label) {
//                 labels.push_back(label);
//             }
//         }
//         labels.sort(); // Lexicographical sort for the rest

//         for (const auto& label : labels) {
//             if (!func.body.count(label)) continue; // Should not happen
//             const auto& bb = func.body.at(label);
//             os << "\n" << label << ":\n";
//             for (const auto& inst : bb.insts) {
//                 os << inst;
//             }
//             os << bb.term;
//         }
//         os << "}\n\n";
//     }
//     return os;
// }

} // namespace LIR