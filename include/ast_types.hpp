#ifndef AST_TYPES_HPP
#define AST_TYPES_HPP

#include "std.hpp"

namespace ast {
    
    struct Id {
        String   val;
        Location loc;

        friend ostream& operator<<(ostream& os, const Id& id)
        {
            os << id.val;
            return os;
        }
    };

    struct Int_Lit {
        int      val;
        Location loc;

        friend ostream& operator<<(ostream& os, const Int_Lit& ilit)
        {
            os << ilit.val;
            return os;
        }
    };

    struct Float_Lit {
        double   val;
        Location loc;

        friend ostream& operator<<(ostream& os, const Float_Lit& flit)
        {
            os << flit.val;
            return os;
        }
    };

    struct String_Lit {
        String   val;
        Location loc;

        friend ostream& operator<<(ostream& os, const String_Lit& slit)
        {
            os << slit.val;
            return os;
        }

    };

    struct Char_Lit {
        char     val;
        Location loc;

        friend ostream& operator<<(ostream& os, const Char_Lit& clit)
        {
            os << clit.val;
            return os;
        }
    };

#define TYPE_LIST(X) \
    X(PRIM, prim, Id*) \
    X(FN, fn, struct { Type* l; Type* r; })

#define MAKE_ENUM_VAR(cons, var, type) \
    cons,

#define MAKE_ENUM(LIST) \
    enum { \
        LIST(MAKE_ENUM_VAR) \
    } _type;

#define MAKE_UNION_VAR(cons, var, type) \
    type var;

#define MAKE_UNION(LIST) \
    union { \
        LIST(MAKE_UNION_VAR) \
    };

    struct Type {
        MAKE_ENUM(TYPE_LIST)
        MAKE_UNION(TYPE_LIST)

        Type(Id* _prim)
            : _type(PRIM), prim(_prim) {}

        Type(Type* _l, Type* _r)
            : _type(FN), fn({ _l, _r }) {}

        ~Type()
        {
            switch(_type) {
            case PRIM:
                DELETE_PTR(prim);
                break;
            case FN:
                DELETE_PTR(fn.l);
                DELETE_PTR(fn.r);
                break;
            }
        }

        friend ostream& operator<<(ostream& os, const Type& type) 
        {
            switch (type._type) {
            case PRIM:
                os << *type.prim;
                break;
            case FN:
                os << "(" << *type.fn.l << " -> " << *type.fn.r << ")";
                break;
            }
            return os;
        }
    };

    struct Field {
        uptr<Id>   id;
        uptr<Type> ty;

        Field(Id* _id, Type* _ty)
            : id(_id), ty(_ty) {}

        friend ostream& operator<<(ostream& os, const Field& field) 
        {
            os << *field.id << " : " << *field.ty;
            return os;
        }
    };

    struct Record {
        uptr<Id>           id;
        Array<uptr<Field>> fields;

        Record(Id* _id) 
            : id(_id) {}

        void add_field(Field* field)
        {
            fields.emplace_back(field);
        }

        friend ostream& operator<<(ostream& os, const Record& record)
        {
            os << "RECORD " << *record.id << endl;
            for (auto& field : record.fields) {
                os << *field << endl;
            }
            os << "END_RECORD";
            return os;
        }
    };

    struct Variant {
        uptr<Id>            id;
        Array<uptr<Record>> records;

        Variant(Id* _id)
            : id(_id) {}

        void add_record(Record* record)
        {
            records.emplace_back(record);
        }

        friend ostream& operator<<(ostream& os, const Variant& variant)
        {
            os << "VARIANT " << *variant.id << endl;
            for (auto& record : variant.records) {
                os << *record << endl;
            }
            os << "END_VARIANT";
            return os;
        }
    };

    struct Data {
        enum T {
            RECORD,
            VARIANT,
        } _type;

        union {
            Record* rec;
            Variant* var;
        };

        Data(Record* _rec)
            : _type(RECORD), rec(_rec) {}

        Data(Variant* _var)
            : _type(VARIANT), var(_var) {}
        
        ~Data()
        {
            switch(_type) {
            case RECORD:
                DELETE_PTR(rec);
                break;
            case VARIANT:
                DELETE_PTR(var);
                break;
            }
        }

        friend ostream& operator<<(ostream& os, const Data& data)
        {
            switch (data._type) {
            case RECORD:
                os << *data.rec;
                break;
            case VARIANT:
                os << *data.var;
                break;
            }
            return os;
        }
    };

    struct Exp {
        enum {
            PINT,
            PFLOAT,
            PCHAR,
            PSTRING,
            SEQ,
            APP,
            REF,
            LET,
            LAM,
            COND,
            MATCH
        } _type;

        struct Alter {
            uptr<Id>  con;
            uptr<Id>  var;
            uptr<Exp> exp;

            Alter(Id* _con, Id* _var, Exp* _exp) 
                : con(_con), var(_var), exp(_exp) {}

            friend ostream& operator<<(ostream& os, const Alter& alter)
            {
                os << *alter.con << " " << *alter.var << " -> " << *alter.exp;
                return os;
            }
        };
 
        union {
            Int_Lit*    ilit;
            Float_Lit*  flit;
            Char_Lit*   clit;
            String_Lit* slit;
            struct {
                Exp* l;
                Exp* r;
            } seq;
            struct {
                Exp* l;
                Exp* r;
            } app;
            Id* ref;
            struct {
                Id* var;
                Exp* val;
            } let;
            struct {
                Id* var;
                Exp* exp;
            } lam;
            struct {
                Exp* c;
                Exp* t;
                Exp* f;
            } cond;
            struct {
                Exp* exp;
                Array<uptr<Alter>>* alter;
            } match;
        };

        Exp() = default;

        static inline Exp* Ilit(Int_Lit* _ilit)
        {
            Exp* e = new Exp {};
            e->_type = PINT; 
            e->ilit = _ilit;
            return e;
        }

        static inline Exp* Flit(Float_Lit* _flit)
        {
            Exp* e = new Exp {};
            e->_type = PFLOAT;
            e->flit = _flit;
            return e;
        }

        static inline Exp* Slit(String_Lit* _slit)
        {
            Exp* e = new Exp {};
            e->_type = PSTRING;
            e->slit = _slit;
            return e;
        }

        static inline Exp* Clit(Char_Lit* _clit)
        {
            Exp* e = new Exp {};
            e->_type = PCHAR;
            e->clit = _clit;
            return e;
        }

        static inline Exp* Seq(Exp* _l, Exp* _r) 
        {
            Exp* e = new Exp {};
            e->_type = SEQ;
            e->seq = { _l, _r };
            return e;
        }

        static inline Exp* App(Exp* _l, Exp* _r) 
        {
            Exp* e = new Exp {};
            e->_type = APP;
            e->app = { _l, _r };
            return e;
        }
        
        static inline Exp* Ref(Id* _ref)
        {
            Exp* e = new Exp {};
            e->_type = REF;
            e->ref = _ref;
            return e;
        }

        static inline Exp* Let(Id* _var, Exp* _val)
        {
            Exp* e = new Exp {};
            e->_type = LET;
            e->let = { _var, _val };
            return e;
        }

        static inline Exp* Lam(Id* _var, Exp* _exp)
        {
            Exp* e = new Exp {};
            e->_type = LAM;
            e->lam = { _var, _exp };
            return e;
        }

        static inline Exp* Cond(Exp* _c, Exp* _t, Exp* _f)
        {
            Exp* e = new Exp {};
            e->_type = COND;
            e->cond = { _c, _t, _f };
            return e;
        }

        static inline Exp* Match(Exp* _exp, Array<uptr<Alter>>* _alter)
        {
            Exp* e = new Exp {};
            e->_type = MATCH;
            e->match = { _exp, _alter };
            return e;
        }

        ~Exp()
        {
            switch (_type) {
                case PINT:
                    DELETE_PTR(ilit);
                    break;
                case PFLOAT:
                    DELETE_PTR(flit);
                    break;
                case PCHAR:
                    DELETE_PTR(clit);
                    break;
                case PSTRING:
                    DELETE_PTR(slit);
                    break;
                case SEQ:
                    DELETE_PTR(seq.l);
                    DELETE_PTR(seq.r);
                    break;
                case APP:
                    DELETE_PTR(app.l);
                    DELETE_PTR(app.r);
                    break;
                case REF:
                    DELETE_PTR(ref);
                    break;
                case LET:
                    DELETE_PTR(let.val);
                    DELETE_PTR(let.var);
                    break;
                case LAM:
                    DELETE_PTR(lam.exp);
                    DELETE_PTR(lam.var);
                    break;
                case COND:
                    DELETE_PTR(cond.c);
                    DELETE_PTR(cond.t);
                    DELETE_PTR(cond.f);
                    break;
                case MATCH:
                    DELETE_PTR(match.exp);
                    DELETE_PTR(match.alter);
                    break;
            }
        }

        friend ostream& operator<<(ostream& os, const Exp& exp)
        {
            switch (exp._type) {
            case PINT:
                os << *exp.ilit;
                break;
            case PFLOAT:
                os << *exp.flit;
                break;
            case PCHAR:
                os << *exp.clit;
                break;
            case PSTRING:
                os << *exp.slit;
                break;
            case SEQ:
                os << *exp.seq.l << endl << " ; " << *exp.seq.r;
                break;
            case APP:
                os << "(" << *exp.app.l << " " << *exp.app.r << ")";
                break;
            case REF:
                os << *exp.ref;
                break;
            case LET:
                os << *exp.let.var << " := " << *exp.let.val;
                break;
            case LAM:
                os << " \\ " << *exp.lam.var << " -> " << *exp.lam.exp;
                break;
            case COND:
                os << " IF " << *exp.cond.c << " THEN " << *exp.cond.t << " ELSE " << *exp.cond.f << " END";
                break;
            case MATCH:
                os << " MATCH " << *exp.match.exp << " WITH ";
                for (auto& alter : *exp.match.alter) {
                    os << endl << *alter;
                }
                break;
            }
            return os;
        }
    };

    struct Comb {
        uptr<Id>           id;
        Array<uptr<Field>> args;
        uptr<Type>         ty;
        uptr<Exp>          val;

        Comb(Id* _id, Type* _ty, Exp* _val)
            : id(_id), ty(_ty), val(_val) {}

        void add_arg(Field* field) {
            args.emplace_back(field);
        }

        
        friend ostream& operator<<(ostream& os, const Comb& comb)
        {
            os << "COMB " << *comb.id << endl;
            os << "( |";
            for (auto& arg : comb.args) {
                os << *arg << "| ";
            }
            os << ")";
            os << " : " << *comb.ty << " = " << endl;
            os << *comb.val << endl;
            os << "END_COMB";
            return os;
        }
    };

    struct Toplevel {
        enum {
            DATA,
            COMB,
        } _type;
        
        union {
            Data* data;
            Comb* comb;
        };

        Toplevel(Data* _data)
            : _type(DATA), data(_data) {}

        Toplevel(Comb* _comb)
            : _type(COMB), comb(_comb) {}

        ~Toplevel()
        {
            switch(_type) {
            case DATA:
                DELETE_PTR(data);
                break;
            case COMB:
                DELETE_PTR(comb);
                break;
            }
        }

        friend ostream& operator<<(ostream& os, const Toplevel& tl)
        {
            switch (tl._type) {
            case DATA:
                os << *tl.data;
                break;
            case COMB:
                os << *tl.comb;
                break;
            }
            return os;
        }
    };

    struct Module {
        File file;
        uptr<Id> id;
        Array<uptr<Toplevel>> tls;

        Module(File _file, Id* _id) 
            : file(_file), id(_id) {}

        void add_tl(Toplevel* tl)
        {
            tls.emplace_back(tl);
        }

        friend ostream& operator<<(ostream& os, const Module& mod)
        {
            os << "MODULE " << *mod.id << endl;
            os << "[ FILE: " << mod.file.to_path() << " ]" << endl;
            for (auto& tl : mod.tls) {
                os << *tl << endl;
            }
            os << "END_MODULE";
            return os;
        }
    };

    struct Unit {
       Array<uptr<Module>> modules;
    };

    struct Token {
        enum T {
            INT_VAL = 0,
            FLOAT_VAL,
            CHAR_VAL,
            STRING_VAL,

            MODULE,

            TYPE,
            DATA,
            FIELD,
            RECORD,
            VARIANT,
            COMB,

            EXP,
            ALTER,
            ALTER_LIST,

            TOPLEVEL,

            ID,

            INT_LIT,
            FLOAT_LIT,
            STRING_LIT,
            CHAR_LIT,

            LIST,
            EMPTY,
        } _type;

        union {
            int int_val;
            double float_val;
            char char_val;
            char* string_val;

            Module* module;
            
            Id* id;
            Int_Lit* int_lit;
            Float_Lit* float_lit;
            Char_Lit* char_lit;
            String_Lit* string_lit;

            Type* type;
            Field* field;
            Record* record;
            Variant* variant;
            Data* data;
            Comb* comb;
            Exp* exp;
            Exp::Alter* alter;
            Array<uptr<Exp::Alter>>* alter_list;
            Toplevel* toplevel;

            Array<void*> *list;

            void* empty;
            void* __set;
        };

        void set(T t, void *s)
        {
            this->_type = t;
            this->__set = s;
        }
    };
}

#endif