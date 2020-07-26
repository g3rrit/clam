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

    struct Type {

#define TYPE_LIST(X, Y)     \
    X(Prim, Id*)            \
    X(Fun, Type*, Type*)

        MAKE_VARIANT(Type, TYPE_LIST)

        friend ostream& operator<<(ostream& os, Type& type) 
        {
            visit(overload {
                [&] (const Prim& prim) {
                    os << *get<0>(prim);
                },
                [&] (const Fun& fun) {
                    os << "(" << *get<0>(fun) << " -> " << *get<1>(fun) << ")";
                }
            }, type);
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

#define DATA_LIST(X, Y) \
    Y(Record*) \
    Y(Variant*)

        MAKE_VARIANT(Data, DATA_LIST)

        friend ostream& operator<<(ostream& os, Data& data)
        {
            visit(overload {
                [&] (const Record* r) {
                    os << *r;
                },
                [&] (const Variant* v) {
                    os << *v;
                }
            }, data);
            return os;
        }
    };

    struct Exp {

#define EXP_LIST(X, Y) \
    Y(Int_Lit*) \
    Y(Float_Lit*) \
    Y(Char_Lit*) \
    Y(String_Lit*) \
    Y(Id*) \
    X(Seq, Exp*, Exp*) \
    X(App, Exp*, Exp*) \
    X(Let, Id*, Exp*) \
    X(Lam, Id*, Exp*) \
    X(Cond, Exp*, Exp*, Exp*) \
    X(Match, Exp*, Array<uptr<Alter>>*)

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
 
        MAKE_VARIANT(Exp, EXP_LIST)

        friend ostream& operator<<(ostream& os, Exp& exp)
        {
            visit(overload {
                [&] (const Int_Lit* ilit) {
                    os << *ilit;
                },
                [&] (const Float_Lit* flit) {
                    os << *flit;
                },
                [&] (const Char_Lit* clit) {
                    os << *clit;
                },
                [&] (const String_Lit* slit) {
                    os << *slit;
                },
                [&] (const Id* ref) {
                    os << *ref;
                },
                [&] (const Seq& seq) {
                    os << *get<0>(seq) << endl << " ; " << *get<1>(seq);
                },
                [&] (const App& app) {
                    os << " ( " << *get<0>(app) << " " << *get<1>(app) << " ) ";
                },
                [&] (const Let& let) {
                    os << *get<0>(let) << " := " << *get<1>(let);
                },
                [&] (const Lam& lam) {
                    os << " \\ " << *get<0>(lam) << " -> " << *get<1>(lam);
                },
                [&] (const Cond& cond) {
                    os << "IF " << *get<0>(cond) << endl << " THEN " <<  *get<1>(cond) << endl << " ELSE " << *get<2>(cond) << " END_IF";
                },
                [&] (const Match& match) {
                    os << "MATCH " << *get<0>(match) << endl;
                    for (auto& alt : *get<1>(match)) {
                        os << *alt << endl;
                    }
                    os << "END_MATCH";
                },
            }, exp);
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

#define TOPLEVEL_LIST(X, Y) \
    Y(Data*) \
    Y(Comb*)

        MAKE_VARIANT(Toplevel, TOPLEVEL_LIST)

        friend ostream& operator<<(ostream& os, Toplevel& tl)
        {
            visit(overload {
                [&] (Data* data) {
                    os << *data;
                },
                [&] (Comb* comb) {
                    os << *comb;
                }
            }, tl);
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