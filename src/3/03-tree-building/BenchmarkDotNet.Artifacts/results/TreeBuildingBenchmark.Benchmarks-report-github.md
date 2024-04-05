``` ini

BenchmarkDotNet=v0.12.1, OS=macOS 13.6.3 (22G436) [Darwin 22.6.0]
Intel Core i7-7920HQ CPU 3.10GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=8.0.201
  [Host]     : .NET Core 8.0.2 (CoreCLR 8.0.224.6711, CoreFX 8.0.224.6711), X64 RyuJIT DEBUG
  DefaultJob : .NET Core 8.0.2 (CoreCLR 8.0.224.6711, CoreFX 8.0.224.6711), X64 RyuJIT


```
|   Method |     Mean |     Error |    StdDev |   Median | Ratio | RatioSD |  Gen 0 | Gen 1 | Gen 2 | Allocated |
|--------- |---------:|----------:|----------:|---------:|------:|--------:|-------:|------:|------:|----------:|
| Baseline | 8.227 μs | 0.2027 μs | 0.5618 μs | 8.147 μs |  1.00 |    0.00 | 3.3646 |     - |     - |  13.75 KB |
|     Mine | 4.889 μs | 0.1787 μs | 0.5039 μs | 4.705 μs |  0.60 |    0.07 | 1.8768 |     - |     - |   7.68 KB |
