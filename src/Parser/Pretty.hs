{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser.Pretty where

import Prelude hiding ((<>))
import Text.PrettyPrint
import Data.List (intersperse)
import Parser.AST 

class Pretty t where 
  pp :: t -> Doc

pretty :: Pretty t => t -> Doc
pretty t = pp t

instance Pretty Module where
  pp (Module m ts) = (text "Module") <+> (text m) $$
    (vcat $ map (\case
      Left c  -> pp c
      Right d -> pp d) ts)

instance Pretty Data where
  pp (Data n ns vs) = 
    "data" <+> (text n) <+> (hsep $ map text ns) 
      $$ (nest 2 $ vcat $ map (pp) vs)

instance Pretty Comb where
  pp (Comb n ns t e) = 
    "let" <+> (text n) <+> (vcat $ map text ns) <+> colon <+> (pp t) 
      $$ (nest 2 $ equals <+> (pp e))

instance Pretty Variant where
  pp (Variant n t) = (text n) <+> (hsep $ map pp t)

instance Pretty Alter where
  pp (n, ns, e) = (text "|") <+> (text n) <+> (hsep $ map text ns) <+> (text "->")
    <+> (pp e)

instance Pretty Exp where
  pp = \case 
    EVar n -> text n
    EPrim p -> pp p
    ESeq e0 e1 -> lparen <+> (pp e0) $$ (text ";") <+> (pp e1) <+> rparen
    ELet n mt e -> (text n) <+> colon <+> (maybe (text "undef") pp mt) <+> equals <+> (pp e)
    EConst n -> text n 
    EIf c t e -> (text "if") <+> (pp c) $$ (text "then") <+> (pp t) $$ (text "else") <+> (pp e)
    EAp e0 e1 -> lparen <> (pp e0) <+> (pp e1) <> rparen
    ELam ns e -> lbrack <+> (hsep $ map text ns) <+> (text "->") <+> (pp e) <+> rbrack
    ECase e as -> (text "match") <+> (pp e) $$ (nest 2 $ vcat $ map pp as)

instance Pretty Prim where
  pp (PInt i) = int i

instance Pretty Type where
  pp = \case 
    TFn t0 t1 -> lparen <> (pp t0) <+> (text "->") <+> (pp t1) <> rparen
    TPrim n -> text n
    TKind t0 t1 -> lparen <+> pp t0 <+> pp t1 <+> rparen
    TGen n -> text n
