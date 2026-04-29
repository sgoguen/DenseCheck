# Dafny Scripts

Dafny is often installed with the Visual Studio IDE, but it can also be used from the command line. The scripts in this directory are intended to be run from the command line, and they will check the Dafny files in this directory and its subdirectories.

When I run the compile script from the IDE, it constructs the following command line:

```
& "C:\Program Files\dotnet\dotnet.EXE" "c:\Users\Stephen Goguen\.vscode\extensions\dafny-lang.ide-vscode-3.5.2\out\resources\4.11.0\github\dafny\Dafny.dll" "build" "--target:go" "c:\Projects\github.com\sgoguen\DenseCheck\src\Dafny\Countable.dfy"
```

But let's say we wanted a simple Powershell script to run this and we wanted it 
to work on anyone's machine, not just mine. We could write a script like this:

```powershell
$DafnyPath = "C:\Program Files\dotnet\dotnet.EXE"
# This following would be a mistake, because it would only work on my machine, not anyone else's
$DafnyDll = "c:\Users\Stephen Goguen\.vscode\extensions\dafny-lang.ide-vscode-3.5.2\out\resources\4.11.0\github\dafny\Dafny.dll"
```

Let's look at our environment variables to see if we can find a better way to specify the path to the Dafny DLL. We can use the `env` command in Powershell to list all environment variables:

```powershell
env
```

Q: How do we get all environment variables in Powershell?
A: You can use the `env` command to list all environment variables in Powershell.

Q: That doesn't work.  That's bash.
A: You're right, `env` is a bash command. In Powershell, you can use `Get-ChildItem Env:` to list all environment variables.