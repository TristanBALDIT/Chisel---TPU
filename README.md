# Chisel-TPU

Parameterized Weight-Stationary Tensor Processing Unit (TPU) implemented in Chisel 6 (`Int8Config` / `BF16Config`), targeting FPGA deployment (e.g., Digilent Nexys 4 DDR).

## Matrix Multiplication Convention

The hardware natively computes:

$$C_{M \times N} = A_{M \times K} \times W_{K \times N}$$

All matrices are stored in natural **row-major** order (zero transposes required):
- **$W$ ($K \times N$)**: Stationary inside the Processing Elements (`PE`), pre-loaded from Weight BRAM by `WeightLoader`.
- **$A$ ($M \times K$)**: Streamed from Activation BRAM through the `SkewBuffer` into the left edge of the `SystolicArray` by `ActivationController`.
- **$C$ ($M \times N$)**: Deskewed and buffered across banked accumulators by `SramOutputSystem`, emitting unskewed parallel rows with backpressure (`stall`).

## Package Structure

### Active TPU Packages

| Package | Location | Modules |
| :--- | :--- | :--- |
| `tpu.config` | `src/main/scala/tpu/config/` | `TpuConfig`, `Int8Config`, `BF16Config` |
| `tpu.core` | `src/main/scala/tpu/core/` | `PE`, `SystolicArray` |
| `tpu.input` | `src/main/scala/tpu/input/` | `SkewBuffer`, `ActivationController`, `WeightLoader` |
| `tpu.output` | `src/main/scala/tpu/output/` | `SramOutputSystem` |

### Archived / Legacy Prototypes

| Package | Location | Modules |
| :--- | :--- | :--- |
| `tpu.legacy` | `src/main/scala/tpu/legacy/` | `OutputSystem`, `OutputSystemV2`, `ReverseSkewOutputSystem`, `MemoryWriteController` |

## Running Tests

- **Run all active TPU tests:**
  ```bash
  sbt "testOnly tpu.core.* tpu.input.* tpu.output.*"
  ```
- **Run a single test suite:**
  ```bash
  sbt "testOnly *WeightLoaderTester"
  ```
- **Run everything (including legacy prototypes):**
  ```bash
  sbt test
  ```
