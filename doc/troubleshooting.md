# Troubleshooting

Last Updated: 2026-04-19

适用范围: 新架构（`src/`）与根目录旧流水线。

## 1. `make test` / `racket run-tests.rkt` 失败

### 现象

```text
cannot open module file
module path: cutie-ftree
path: .../vendor/cutie-ftree.rkt/main.rkt
```

### 原因

根目录旧链路包含 `(require cutie-ftree)`，但当前 vendor 结构下没有 `main.rkt` collection 入口文件。

### 处理

1. 新开发优先使用 `src/` 下 `raco test` 流程。
2. 若必须跑旧链路：统一 `cutie-ftree` require 方式，或补齐 collection 入口。

## 2. Java 管线测试找不到 `ClassTransform.dat`

### 现象

`src/pipeline/java/pipeline-test.rkt` 或 transform 测试报 fixture 缺失。

### 检查

```bash
ls test/integration/ClassTransform.dat
```

### 处理

```bash
racket src/frontend/java/java-tools/build.rkt --download
racket src/frontend/java/java-tools/build.rkt --compile
racket src/frontend/java/java-tools/build.rkt --analyze com/cutiedeng/ClassTransform --output test/integration/ClassTransform.dat
```

## 3. `raco test` 命令通过但 `make test` 失败

### 原因

两套链路并存：
- `src/` 新架构测试使用模块级 `rackunit`。
- 根目录 `make test` 走旧课程链路，依赖条件不同。

### 处理

以你改动所在链路为准：
- 改 `src/*` -> 以 `raco test src/...` 为主验证。
- 改根目录旧链路 -> 必须修通 `make test`。

## 4. Java 工具执行失败（`javac` / `java` / `curl`）

### 现象

`build.rkt` 在下载、编译或分析阶段失败。

### 检查

```bash
which curl
which javac
which java
java -version
javac -version
```

### 处理

1. 安装或修正 JDK。
2. 确保命令在 PATH 中。
3. 在网络受限环境下，预先准备 `src/frontend/java/java-tools/libs/*.jar`。

## 5. 相对路径导致测试运行失败

### 现象

fixture 路径解析错误，尤其在非仓库根目录执行时。

### 处理

1. 始终从仓库根目录执行测试命令。
2. 使用文档中的完整相对路径命令，不省略前缀。

## 6. `.md` 文档未出现在 `git status`

### 现象

新增文档后看不到变更。

### 原因

`.gitignore` 存在全局 `*.md` 规则。

### 处理

确保有白名单：

```gitignore
!doc/
!doc/*.md
```

## 7. 建议的最小自检顺序

```bash
raco test src/driver/graph/graph-test.rkt
raco test src/component/java/transform/cfg-to-rvsdg-test.rkt
raco test src/pipeline/java/pipeline-test.rkt
```

若这三条通过，再按改动范围补充其他测试。

## 8. 相关阅读

- [quickstart.md](/Users/cutiedeng/Y2026/M03/D28/compiler.rkt/doc/quickstart.md)
- [testing.md](/Users/cutiedeng/Y2026/M03/D28/compiler.rkt/doc/testing.md)
