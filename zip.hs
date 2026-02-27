type ListF y x = [x -> y]
type Yoneda y x = x -> y

pureL :: forall y. forall x. Yoneda y x -> ListF y x
pureL = pure

bindL :: forall x. forall y. (forall z. Yoneda x z -> ListF y z) -> (forall z. ListF x z -> ListF y z)
bindL alpha fs =
  let gs = alpha id in fmap (uncurry (.)) (zip gs fs)

-- equivalently zipWith (.) (alpha id) fs. cf Free Applicatives paper talking about "lifting functions with more than one param."
