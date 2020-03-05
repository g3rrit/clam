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

instance Pretty Template where
  pp (Template ts) =
    "<" <+> (hsep $ map text ts) <+> ">"

instance Pretty Data where
  pp (Data n tp ns vs _) =
      "data" <+> (text n) <+> (maybe "" pp tp) <+> (hsep $ map text ns)
      $$ (nest 2 $ vcat $ map (pp) vs)

instance Pretty Comb where
  pp (Comb n tp ns t e _) =
      "let" <+> (text n) <+> (maybe "" pp tp) <+> (vcat $ map text ns) <+> colon <+> (pp t)
      $$ (nest 2 $ equals <+> (pp e))

instance Pretty Variant where
  pp (Variant n t _) = (text n) <+> (hsep $ map pp t)

instance Pretty Alter where
  pp (Alter n ns e _) = (text "|") <+> (text n) <+> (hsep $ map text ns) <+> (text "->")
    <+> (pp e)

instance Pretty Exp where
  pp = \case
    EVar n _ -> text n
    EPrim p _ -> pp p
    EConst n _ -> text n
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
    TKind t0 t1 -> lparen <+> pp t0 <+> pp t1 <+> rparen
    TGen n _ -> text n
    TRef t -> lparen <+> (text "&") <+> pp t <+> rparen
    TSptr t -> lparen <+> (text "*") <+> pp t <+> rparen
    TUptr t -> lparen <+> (text "^") <+> pp t <+> rparen
