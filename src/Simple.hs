import Control.Applicative ((<|>))

infixl 2 :@:

data Term = Idx Int
          | Term :@: Term
          | Lmb Term
          deriving (Eq, Read, Show)

shift :: Int -> Term -> Term
shift d = go 0
  where
    go :: Int -> Term -> Term
    go c (Idx k)           = if k < c then Idx k else Idx (k + d)
    go c (Lmb term)        = Lmb (go (succ c) term)
    go c (term1 :@: term2) = go c term1 :@: go c term2

substDB :: Int -> Term -> Term -> Term
substDB j s (Idx k)           = if k == j then s else Idx k
substDB j s (Lmb term)        = Lmb $ substDB (succ j) (shift 1 s) term
substDB j s (term1 :@: term2) = substDB j s term1 :@: substDB j s term2

betaRuleDB :: Term -> Term
betaRuleDB (Lmb t :@: s) = shift (-1) $ substDB 0 (shift 1 s) t
betaRuleDB _             = undefined

oneStepDBN :: Term -> Maybe Term
oneStepDBN term@(Lmb _ :@: _) = Just $ betaRuleDB term
oneStepDBN (Lmb term)         = Lmb <$> oneStepDBN term
oneStepDBN (term1 :@: term2)  = (:@: term2) <$> oneStepDBN term1
  <|> (term1 :@:) <$> oneStepDBN term2
oneStepDBN _                  = Nothing

oneStepDBA :: Term -> Maybe Term
oneStepDBA term@(Lmb p :@: q) = Just $ maybe (betaRuleDB term) (Lmb p :@:) (oneStepDBA q)
oneStepDBA (Lmb p)            = Lmb <$> oneStepDBA p
oneStepDBA (p :@: q)          = (:@: q) <$> oneStepDBA p <|> (p :@:) <$> oneStepDBA q
oneStepDBA _                  = Nothing

nfDB :: (Term -> Maybe Term) -> Term -> Term 
nfDB oneStepDB term = maybe term (nfDB oneStepDB) (oneStepDB term)

