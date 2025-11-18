#include <iostream>
#include <stdexcept>
#include <fstream>
#include <memory>
#include "json.hpp"
#include "lir.hpp"
#include "codegen.hpp"

using nlohmann::json;
using namespace LIR;

// ============= Type parsing =============

// JSON type shapes you have:
//
// "Int"
// {"Struct": "MyStruct"}
// {"Array": <Type>}
// {"Ptr": <Type>}
// {"Fn": [ [ paramTypes... ], retType ] }
//
// possibly nested, and funptrs use {"Ptr": {"Fn": ...}}

static TypePtr parse_type(const json& jt);

// Simple helpers to construct shared_ptr types
static TypePtr make_int() {
    return std::make_shared<IntType>();
}

static TypePtr parse_fn_type(const json& jfn) {
    // jfn is: [ [ paramTypes... ], retType ]
    const auto& params_json = jfn.at(0);
    const auto& ret_json    = jfn.at(1);

    std::vector<TypePtr> params;
    params.reserve(params_json.size());
    for (const auto& pj : params_json) {
        params.push_back(parse_type(pj));
    }
    TypePtr ret = parse_type(ret_json);

    return std::make_shared<FnType>(std::move(params), ret);
}

static TypePtr parse_type(const json& jt) {
    // base case: string like "Int"
    if (jt.is_string()) {
        std::string s = jt.get<std::string>();
        if (s == "Int") {
            return make_int();
        }
        // If you later have "Bool" or other primitives, handle them here.
        throw std::runtime_error("Unknown base type string: " + s);
    }

    // compound types: object with keys like Struct / Array / Ptr / Fn
    if (jt.is_object()) {
        if (jt.contains("Struct")) {
            std::string sid = jt.at("Struct").get<std::string>();
            return std::make_shared<StructType>(sid);
        }
        if (jt.contains("Array")) {
            TypePtr elem = parse_type(jt.at("Array"));
            return std::make_shared<ArrayType>(elem);
        }
        if (jt.contains("Ptr")) {
            TypePtr elem = parse_type(jt.at("Ptr"));
            return std::make_shared<PtrType>(elem);
        }
        if (jt.contains("Fn")) {
            return parse_fn_type(jt.at("Fn"));
        }
    }

    throw std::runtime_error("Unknown type JSON: " + jt.dump());
}

// ============= Inst / Terminal parsing =============

static ArithOp parse_arith_op(const std::string& s) {
    if (s == "Add") return ArithOp::Add;
    if (s == "Sub") return ArithOp::Sub;
    if (s == "Mul") return ArithOp::Mul;
    if (s == "Div") return ArithOp::Div;
    throw std::runtime_error("Unknown ArithOp: " + s);
}

static RelOp parse_rel_op(const std::string& s) {
    if (s == "Eq")  return RelOp::Eq;
    if (s == "NotEq") return RelOp::NotEq;
    if (s == "Lt")  return RelOp::Lt;
    if (s == "Lte") return RelOp::Lte;
    if (s == "Gt")  return RelOp::Gt;
    if (s == "Gte") return RelOp::Gte;
    throw std::runtime_error("Unknown RelOp: " + s);
}

// One instruction is encoded as: { "<Kind>": { ...fields... } }
static Inst parse_inst(const json& j) {
    if (!j.is_object() || j.size() != 1) {
        throw std::runtime_error("Instruction JSON must have exactly one key: " + j.dump());
    }

    auto it = j.begin();
    const std::string kind = it.key();
    const json& data = it.value();

    if (kind == "Const") {
        Const c;
        c.lhs = data.at("lhs").get<VarId>();
        c.val = data.at("rhs").get<int>();
        return c;
    } else if (kind == "Copy") {
        Copy c;
        c.lhs = data.at("lhs").get<VarId>();
        c.op  = data.at("rhs").get<VarId>();
        return c;
    } else if (kind == "Arith") {
        Arith a;
        a.lhs   = data.at("lhs").get<VarId>();
        a.aop   = parse_arith_op(data.at("aop").get<std::string>());
        a.left  = data.at("left").get<VarId>();
        a.right = data.at("right").get<VarId>();
        return a;
    } else if (kind == "Cmp") {
        Cmp c;
        c.lhs   = data.at("lhs").get<VarId>();
        c.rop   = parse_rel_op(data.at("rop").get<std::string>());
        c.left  = data.at("left").get<VarId>();
        c.right = data.at("right").get<VarId>();
        return c;
    } else if (kind == "Load") {
        Load l;
        l.lhs = data.at("lhs").get<VarId>();
        l.src = data.at("src").get<VarId>();
        return l;
    } else if (kind == "Store") {
        Store s;
        s.dst = data.at("dst").get<VarId>();
        s.op  = data.at("op").get<VarId>();
        return s;
    } else if (kind == "Gfp") {
        Gfp g;
        g.lhs   = data.at("lhs").get<VarId>();
        g.src   = data.at("src").get<VarId>();
        g.sid   = data.at("sid").get<StructId>();
        g.field = data.at("fld").get<FieldId>();
        return g;
    } else if (kind == "Gep") {
        Gep g;
        g.lhs     = data.at("lhs").get<VarId>();
        g.src     = data.at("src").get<VarId>();
        g.idx     = data.at("idx").get<VarId>();
        g.checked = data.at("checked").get<bool>();
        return g;
    } else if (kind == "AllocSingle") {
        AllocSingle a;
        a.lhs = data.at("lhs").get<VarId>();
        a.typ = parse_type(data.at("typ"));
        return a;
    } else if (kind == "AllocArray") {
        AllocArray a;
        a.lhs = data.at("lhs").get<VarId>();
        a.amt = data.at("amt").get<VarId>();
        a.typ = parse_type(data.at("typ"));
        return a;
    } else if (kind == "Call") {
        Call c;
        // lhs can be null (void call)
        if (data.contains("lhs") && !data.at("lhs").is_null()) {
            c.lhs = data.at("lhs").get<VarId>();
        }
        c.callee = data.at("callee").get<VarId>();
        for (const auto& aj : data.at("args")) {
            c.args.push_back(aj.get<VarId>());
        }
        return c;
    }

    throw std::runtime_error("Unknown instruction kind: " + kind);
}

// Terminals:
//
// "term": { "Jump": "lbl" }
// "term": { "Branch": { "guard": "v", "tt": "l1", "ff": "l2" } }
// "term": { "Ret": "v" } or { "Ret": null } for void

static Terminal parse_terminal(const json& j) {
    if (!j.is_object() || j.size() != 1) {
        throw std::runtime_error("Terminal JSON must have exactly one key: " + j.dump());
    }

    auto it = j.begin();
    const std::string kind = it.key();
    const json& data = it.value();

    if (kind == "Jump") {
        Jump t;
        t.target = data.get<BbId>();
        return t;
    } else if (kind == "Branch") {
        Branch b;
        b.guard = data.at("guard").get<VarId>();
        b.tt    = data.at("tt").get<BbId>();
        b.ff    = data.at("ff").get<BbId>();
        return b;
    } else if (kind == "Ret") {
        Ret r;
        if (!data.is_null()) {
            r.val = data.get<VarId>();
        }
        return r;
    }

    throw std::runtime_error("Unknown terminal kind: " + kind);
}

// ============= BasicBlock / Function / Program parsing =============

static BasicBlock parse_basic_block(const BbId& label, const json& jbb) {
    BasicBlock bb;
    bb.label = label;

    const auto& insts_json = jbb.at("insts");
    for (const auto& ij : insts_json) {
        bb.insts.push_back(parse_inst(ij));
    }

    bb.term = parse_terminal(jbb.at("term"));
    return bb;
}

static Function parse_function(const json& jfn) {
    Function fn;
    fn.name   = jfn.at("name").get<FuncId>();
    fn.rettyp = parse_type(jfn.at("rettyp"));

    // prms: [ [ name, type ], ... ]
    const auto& prms = jfn.at("prms");
    for (const auto& pj : prms) {
        VarId vname = pj.at(0).get<VarId>();
        TypePtr vty = parse_type(pj.at(1));
        fn.params.emplace_back(vname, vty);
    }

    // locals: { varName: typeJson, ... }
    const auto& locals_json = jfn.at("locals");
    for (auto it = locals_json.begin(); it != locals_json.end(); ++it) {
        VarId vname = it.key();
        TypePtr vty = parse_type(it.value());
        fn.locals.emplace(vname, vty);
        fn.locals_order.push_back(vname); // Preserve JSON insertion order
    }

    // body: { label: { insts: [...], term: {...} }, ... }
    const auto& body_json = jfn.at("body");
    for (auto it = body_json.begin(); it != body_json.end(); ++it) {
        BbId label = it.key();
        BasicBlock bb = parse_basic_block(label, it.value());
        fn.body.emplace(label, std::move(bb));
    }

    return fn;
}

static Program parse_program(const json& j) {
    Program prog;

    // structs: { name: { fieldName: typeJson, ... }, ... }
    if (j.contains("structs")) {
        const auto& structs_json = j.at("structs");
        for (auto it = structs_json.begin(); it != structs_json.end(); ++it) {
            StructId sid = it.key();
            const auto& fields_json = it.value();

            Struct s;
            s.name = sid;
            for (auto fit = fields_json.begin(); fit != fields_json.end(); ++fit) {
                FieldId fid = fit.key();
                TypePtr fty = parse_type(fit.value());
                s.fields.emplace(fid, fty);
            }
            prog.structs.emplace(sid, std::move(s));
        }
    }

    // externs: { name: FnTypeJson, ... }
    if (j.contains("externs")) {
        const auto& ex_json = j.at("externs");
        for (auto it = ex_json.begin(); it != ex_json.end(); ++it) {
            FuncId name = it.key();
            TypePtr ty  = parse_type(it.value());   // should be FnType
            prog.externs.emplace(name, ty);
        }
    }

    // funptrs: { name: Ptr(FnTypeJson), ... }
    if (j.contains("funptrs")) {
        const auto& fp_json = j.at("funptrs");
        for (auto it = fp_json.begin(); it != fp_json.end(); ++it) {
            FuncId name = it.key();
            TypePtr ty  = parse_type(it.value());   // Ptr(Fn(...))
            prog.funptrs.emplace(name, ty);
        }
    }

    // functions: { name: <fnObj>, ... }
    const auto& fns_json = j.at("functions");
    for (auto it = fns_json.begin(); it != fns_json.end(); ++it) {
        const json& jfn = it.value();
        Function fn = parse_function(jfn);
        prog.functions.emplace(fn.name, std::move(fn));
    }

    return prog;
}

// ============= main =============

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <file.astj>\n";
        return 1;
    }
    
    // 1. Open and read the input file
    std::ifstream input_file(argv[1]);
    if (!input_file.is_open()) {
        std::cerr << "Error: Could not open file " << argv[1] << "\n";
        return 1;
    }

    json j;

    try {
        j = nlohmann::json::parse(input_file);
    } catch (nlohmann::json::parse_error& e) {
        std::cerr << "Error: Failed to parse JSON.\n" << e.what() << std::endl;
        return 1;
    }

    try {
        Program prog = parse_program(j);

        Codegen cg(std::cout);  // codegen pass
        cg.emit_program(prog);
    } catch (const std::exception& ex) {
        std::cerr << "codegen error: " << ex.what() << "\n";
        return 1;
    }
    return 0;
}
