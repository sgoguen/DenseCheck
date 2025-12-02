module InstanceCatalog.Functions


/// <summary>
/// Builds a *memoized recursive function* from an open-recursive function body.
/// </summary>
/// 
/// <remarks>
/// <para>
/// This helper exists because memoizing a normal `let rec` function is awkward:
/// the recursive call is baked into the function definition, so you can't wrap
/// the function with a cache.  `memoizeRec` solves this by having you write your
/// function in *open recursion* style—i.e., your function takes its own
/// recursive reference as its first argument.
/// </para>
///
///     ('a -> 'b) -> 'a -> 'b
///
/// You write the function body assuming:  
///   - The first parameter is “myself”, a function you may call recursively.  
///   - The second parameter is your actual input.
///
/// `memoizeRec` then seals this open-recursive body into a closed, memoized
/// function.  The returned function behaves like any ordinary recursive
/// function, except repeated calls with the same input `'a` are returned
/// instantly from a cache.
///
/// <example>
/// <code>
///
///     let fib =
///         memoizeRec (fun self n ->
///             if n <= 1 then n
///             else self (n - 1) + self (n - 2))
///
///     fib 40   // runs instantly due to memoization
///
/// </code>
/// </example>
///
/// <para>
/// This pattern is especially useful for expensive recursive computations,
/// dynamic programming, interpreters, and any recursion with significant
/// overlapping subproblems.
/// </para>
/// </remarks>
let memoizeRec (f: ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
    // Cache for computed results.  Lazy ensures we don't evaluate
    // the recursive branch eagerly when inserting into the dictionary.
    let cache =
        System.Collections.Concurrent.ConcurrentDictionary<'a, Lazy<'b>>()

    // `recF` is the recursive function we ultimately return.
    // It calls the user-supplied body `f`, passing itself (`recF`)
    // as the recursive reference.
    let rec recF (x: 'a) : 'b =
        cache.GetOrAdd(x, lazy f recF x).Value

    recF