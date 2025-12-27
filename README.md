# Hikari-LLVM19

基于 [Hikari](https://github.com/HikariObfuscator/Hikari) 移植到 LLVM 19 的代码混淆器，适用于 Android NDK r28。

## 使用方法

- **-mllvm -enable-allobf**     全部启用
- **-mllvm -enable-bcfobf**     虚假控制流
- **-mllvm -enable-cffobf**     控制流平坦化
- **-mllvm -enable-splitobf**   基本块分割
- **-mllvm -enable-subobf**     指令替换
- **-mllvm -enable-indibran**   间接跳转
- **-mllvm -enable-strcry**     字符串加密
- **-mllvm -enable-funcwra**    函数封装

## 安装

### 1. 拉取 LLVM 源代码

```bash
git clone --config core.autocrlf=false https://github.com/llvm/llvm-project.git
cd llvm-project
git checkout llvmorg-19.1.0
```

### 2. 合并源码

将本项目的 `llvm` 和 `clang` 文件夹复制到 `llvm-project` 目录下（覆盖合并）

### 3. 编译

```cmd
build.cmd
```

注: 需要先修改 `build.cmd` 中的 `vcvars64.bat` 路径为你本机的 Visual Studio 安装路径

### 4. 替换 NDK

将 `llvm-project/build` 目录下的 `bin` 和 `lib` 文件夹复制到：

```
<你的NDK路径>\toolchains\llvm\prebuilt\windows-x86_64\
```

## 致谢

- [Hikari](https://github.com/HikariObfuscator/Hikari) - 原始混淆器项目
- [OLLVM](https://github.com/obfuscator-llvm/obfuscator) - 混淆技术基础
