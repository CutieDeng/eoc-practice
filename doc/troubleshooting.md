# Troubleshooting

Last Updated: 2026-11-05

适用范围: `src/` 与 `test/`。

## 1. `collection not found` / `cannot open module file racket/pvector`

### 原因

Racket 版本过旧。项目依赖标准库里的 `racket/pvector`、`racket/intmap`、`racket/intbits`、`racket/graph`、`racket/compare`，没有第三方包。

### 处理

升级到 Racket 9.3.13+。

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

## 3. `raco test src test` 报 scribble 缺失

### 原因

`raco test` 扫描目录时会把 `src/component/aarch64/doc/aarch64-sve-asm.scrbl` 当作测试模块，而本机可能没装 `scribble-lib`。

### 处理

用 `make test`（显式文件列表），不要对目录整体 `raco test`。

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

- [quickstart.md](quickstart.md)
- [testing.md](testing.md)
