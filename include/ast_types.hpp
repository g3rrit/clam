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
        enum T {
            PRIM,
            FN,
        } _type;

        union {
            Id* prim;
            struct {
                Type* l;
                Type* r;
            } fn;
        };

        Type(Id* _prim)
            : _type(PRIM), prim(_prim) {}

        Type(Type* _l, Type* _r)
            : _type(FN), fn({ _l, _r }) {}

        ~Type()
        {
            switch(_type) {
            case PRIM:
                delete prim;
                break;
            case FN:
                delete fn.l;
                delete fn.r;
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
                delete rec;
                break;
            case VARIANT:
                delete var;
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
            APP,
            REF,
            
        } _type;
 
        union {
            Int_Lit*    ilit;
            Float_Lit*  flit;
            Char_Lit*   clit;
            String_Lit* slit;
            struct {
                Exp* l;
                Exp* r;
            } app;
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

        static inline Exp* App(Exp* _l, Exp* _r) {
            Exp* e = new Exp {};
            e->_type = APP;
            e->app = { _l, _r };
            return e;
        }

        // todo deconstructor

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
            case APP:
                os << "(" << *exp.app.l << *exp.app.r << ")";
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
            for (auto& arg : comb.args) {
                os << *arg << " ";
            }
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
                delete data;
                break;
            case COMB:
                delete comb;
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
            INT_VAL,
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