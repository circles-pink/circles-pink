type PursUnion = { "$$pursTag": string }

type MatcherFns<U extends PursUnion, Z> = {
    [key in U["$$pursTag"]]: (arg: Extract<U, { "$$pursTag": key }>) => Z
}

type MatcherFnsOrWildcard<U extends PursUnion, Z> =
    MatcherFns<U, Z>
    | Partial<MatcherFns<U, Z>> & { _: () => Z }

export const match = <U extends PursUnion, M>(u: U) => <Z>(matchers: MatcherFnsOrWildcard<U, Z>): Z =>
    matchers[u.constructor.name](u)
