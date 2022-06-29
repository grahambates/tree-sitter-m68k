# tree-sitter-m68k

![build](https://github.com/grahambates/tree-sitter-m68k/actions/workflows/build.yml/badge.svg)
[![npm version](https://img.shields.io/npm/v/tree-sitter-m68k.svg)](https://www.npmjs.com/package/tree-sitter-m68k)

Motorola 68000 family assembly grammar for tree-sitter.

Aims to support syntax and features for [VASM](http://www.compilers.de/vasm.html), as well as native assemblers such as ASMOne and Devpac.

## Usage

### Neovim

With [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) run:

```
:TSInstall m68k
```

## References

- [68000 Programmer's Reference Manual](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
- [VASM Reference Manual](http://sun.hasenbraten.de/vasm/release/vasm_4.html#Mot-Syntax-Module)
- [HiSoft Devpac 3 Manual](https://ia801901.us.archive.org/26/items/DevPacV3.00.Manual/DevPac_v3.00.Manual.pdf)
- [ASMOne Manual](https://archive.org/details/AsmOne1.02Manual)

## License

This package is under a [MIT license](https://github.com/grahambates/tree-sitter-m68k/blob/master/LICENSE.md).
