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
  pp (SData s) = pp s
  pp (PData p) = pp p

instance Pretty SumData where
  pp (SumData n sv _) = (text "data") <+> (text n) 
    $$ (nest 2 $ vcat $ map pp sv)

instance Pretty ProData where
  pp (ProData n fs _) = (text "struct") <+> (text n)
    $$ (nest 2 $ vcat $ map pp fs)

instance Pretty Member where
  pp (Member n t _) = lparen <> (case n of { Left c -> text c; Right i -> integer i})  <+> (text  " : ") <+> (pp t) <> rparen

instance Pretty Comb where
  pp (Comb n ns t e _) =
      "let" <+> (text n) <+> (vcat $ map text ns) <+> colon <+> (pp t)
      $$ (nest 2 $ equals <+> (pp e))

instance Pretty Variant where
  pp (Variant n t _) = (text n) <+> (hsep $ map pp t)

instance Pretty Alter where
  pp (Alter n ns e _) = (text "|") <+> (text n) <+> (hsep $ map text ns) <+> (text "->")
    <+> (pp e)

instance Pretty Exp where
  pp = \case
    EPrim p _ -> pp p
    EVar n _ -> text n
    ELet n mt e _ -> (text n) <+> colon <+> (maybe (text "undef") pp mt) <+> equals <+> (pp e)
    EIf c t e _ -> (text "if") <+> (pp c) $$ (text "then") <+> (pp t) $$ (text "else") <+> (pp e)
    ELam ns e _ -> lbrack <+> (hsep $ map text ns) <+> (text "->") <+> (pp e) <+> rbrack
    ECase e as _ -> (text "match") <+> (pp e) $$ (nest 2 $ vcat $ map pp as)
    EAp e0 e1 -> lparen <> (pp e0) <+> (pp e1) <> rparen
    ESeq e0 e1 -> lparen <+> (pp e0) $$ (text ";") <+> (pp e1) <+> rparen

instance Pretty Prim where
  pp (PInt i) = int i

instance Pretty Type where
  pp = \case
    TFn t0 t1 -> lparen <> (pp t0) <+> (text "->") <+> (pp t1) <> rparen
    TPrim n _ -> text n
