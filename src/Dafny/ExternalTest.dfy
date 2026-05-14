module MyWrapper {
  // The first argument is the class name, the second is the method name
  method {:extern "ExternalUtils", "NativeMethod"} NativeMethod(x: int) returns (y: int)

    class ExternalTest {
        method Test() returns (result: int) {
            result := NativeMethod(42);
        }
    }

}