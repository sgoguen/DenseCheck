// Load the DenseCheck nuget package
#r "nuget: DenseCheck, 0.1.0"

open DenseCheck

// Our Domain
type Role = Admin | Editor | Viewer

type AccessRule =
    | Allow
    | Deny
    | HasRole of Role
    | And of AccessRule * AccessRule
    | Or  of AccessRule * AccessRule
    | Not of AccessRule

// 1. Get our Generator for Rules
// This creates a function: bigint -> AccessRule
let getRule = DenseCheck.getIndex<AccessRule>()

// 2. Generate our Test "Users"

// For now, let's think of a user as the set of Roles they have.
type User = Set<Role>

// We can now ask DenseCheck for the first 100 possible combinations of users (Sets of Roles)
// Since Role only has 3 values, we're more than covered.
let users = DenseCheck.getSetTo<User> 100

// The "Ground Truth" Evaluator
let evaluate (rule: AccessRule) (roles: Set<Role>) : bool =
    let rec eval r =
        match r with
        | Allow -> true
        | Deny -> false
        | HasRole required -> Set.contains required roles
        | And (r1, r2) -> eval r1 && eval r2
        | Or (r1, r2) -> eval r1 || eval r2
        | Not r -> not (eval r)
    eval rule

// We define a Tautology as a rule that returns TRUE for *every* generated user context.
let isTautology rule = 
    users |> List.forall (fun ctx -> evaluate rule ctx)

// And a Contradiction as a rule that returns FALSE for *every* generated user context.
let isContradiction rule = 
    users |> List.forall (fun ctx -> not (evaluate rule ctx))

let huntForSillyExamples() = 

    printfn "--- Scanning for Logic Bugs ---"

    for i in 0I .. 1000I do
        let rule = getRule i
        
        // We ignore the trivial case 'Allow'
        if isTautology rule && rule <> Allow then
            printfn "[%A] Found Complex Tautology: %A" i rule

        // We ignore the trivial case 'Deny'
        if isContradiction rule && rule <> Deny then
            printfn "[%A] Found Complex Contradiction: %A" i rule

huntForSillyExamples()


////////////////////////////////////////// 

// Let's implement a simplify function that reduces our access rule
// to a simpler form.
let simplify = 
    let rec simplifyRec a =
        match a with
        // Double negation
        | Not (Not r) -> simplifyRec r
        // And rules
        
        | And (Allow, r)
        | And (r, Allow) -> simplifyRec r
        | And (Deny, _)
        | And (_, Deny) -> Deny
        // Or rules
        | Or (Deny, r)
        | Or (r, Deny) -> simplifyRec r
        | Or (Allow, _)
        | Or (_, Allow) -> Allow
        // De Morgan
        | Not (And (r1, r2)) ->
            Or (Not (simplifyRec r1), Not (simplifyRec r2)) |> simplifyRec
        | Not (Or (r1, r2)) ->
            And (Not (simplifyRec r1), Not (simplifyRec r2)) |> simplifyRec
        // Push simplification inside
        | And (r1, r2) ->
            let r1' = simplifyRec r1
            let r2' = simplifyRec r2
            if r1' = r1 && r2' = r2 then And (r1', r2') else simplifyRec (And (r1', r2'))
        | Or (r1, r2) ->
            let r1' = simplifyRec r1
            let r2' = simplifyRec r2
            if r1' = r1 && r2' = r2 then Or (r1', r2') else simplifyRec (Or (r1', r2'))
        | Not r ->
            let r' = simplifyRec r
            if r' = r then Not r' else simplifyRec (Not r')
        // Base cases
        | _ -> a
    fun r -> simplifyRec r

let isEquivalent r1 r2 = 
    users |> List.forall (fun ctx -> evaluate r1 ctx = evaluate r2 ctx)

let testSimplify() =
    printfn "--- Verifying Simplification ---"

    for i in 0I .. 1000I do
        let rule1 = getRule i
        let rule2 = simplify rule1
        
        // We ignore the trivial case 'Allow'
        if isEquivalent rule1 rule2 = false then
            printfn "[A] Found Non Equivalent Simplification: %A vs %A" rule1 rule2
            
        // We ignore the trivial case 'Allow'
        if isTautology rule2 && rule2 <> Allow then
            printfn "[%A] Found Complex Tautology: %A" i rule2

        // We ignore the trivial case 'Deny'
        if isContradiction rule2 && rule2 <> Deny then
            printfn "[%A] Found Complex Contradiction: %A" i rule2  

testSimplify()