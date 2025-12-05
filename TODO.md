# TODO

## High Priority

### Rename Project (CRITICAL)
- [x] Rename system and package from "utils" to "iutils" to avoid conflicts
  - The name "utils" is too generic and likely to conflict with other projects
  - Renamed to: `iutils`
  - Updated:
    - [x] System name in .asd files
    - [x] Package name and nickname (now :iu)
    - [x] All references in source files
    - [x] README and documentation
    - [x] Directory name (still needs to be renamed from utils to iutils)

### CI/CD Setup
- [ ] Fix dependencies issues for CI
  - [ ] Resolve `thnappy` dependency (not in standard Quicklisp)
  - [x] Consider removing duplicate HTTP clients (dexador vs drakma)
  - [ ] Fix test framework setup (prove-asdf)
- [ ] Add GitHub Actions workflow
- [ ] Start with simple configuration (Ubuntu + SBCL only)
- [ ] Gradually add more OS and Lisp implementations

### Documentation
- [x] Add docstrings to all exported functions
  - Each function should document: purpose, arguments, return values, and examples
  - Priority modules: `bytes.lisp`, `crypto.lisp`, `strings.lisp`
- [x] Created API.md with detailed function documentation

### Testing
- [x] Expand test coverage (currently only `lists.lisp` has tests)
  - [x] Add tests for `bytes.lisp`
  - [x] Add tests for `strings.lisp`
  - [x] Add tests for `crypto.lisp`
  - [x] Add tests for `hashtables.lisp`
  - [x] Add tests for `time.lisp`
  - [x] Add tests for other modules (`arrays.lisp`, `numbers.lisp`, `sequences.lisp`)
- [ ] Set up code coverage reporting

### Code Quality
- [ ] Fix performance issue in `bytes.lisp` (line 101-102)
  - The `list-of-bytes->bytes` function uses reduce with repeated concatenation
  - Should pre-calculate total length and allocate once
- [ ] Review and implement/remove all TODO comments in source files
- [ ] Address the `setfs-todo.lisp` file - either complete or remove

## Medium Priority

### Project Structure
- [ ] Create `examples/` directory with practical usage examples
  - [ ] Basic usage examples
  - [ ] Crypto operations examples
  - [ ] String manipulation examples
  - [ ] Data conversion examples
- [x] Consider renaming `t/` directory to `tests/` for clarity
- [x] Add API.md with detailed function documentation

### Dependencies
- [ ] Review dependency list and document why each is needed
- [x] Consider consolidating HTTP clients (both dexador and drakma are included)
- [ ] Add version constraints in .asd file where appropriate
- [ ] Fix the endianness issue mentioned in `bit-vector.lisp`

### Code Organization
- [ ] Consider splitting large files (e.g., `lists.lisp` has 333 lines)
- [ ] Add section comments to group related functions
- [ ] Review if all functions in `others.lisp` belong there

## Low Priority

### Enhancements
- [ ] Add CHANGELOG.md to track version changes
- [ ] Consider adding benchmarks for performance-critical functions
- [ ] Add more comprehensive error handling with custom conditions
- [x] Updated package nickname from `:u` to `:iu`

### Nice to Have
- [ ] Add logo/banner for the project
- [ ] Create GitHub Pages documentation
- [ ] Add badges to README (CI status, license, etc.)
- [ ] Consider supporting more Common Lisp implementations in CI

## Completed
- [x] Remove Chinese comments from source files
- [x] Add MIT license
- [x] Create comprehensive README.md
- [x] Add CONTRIBUTING.md
- [x] Add .gitignore
- [x] Fix test file (mklist -> ensure-list)

## Notes
- Some functions like those in `request.lisp` and `yaml.lisp` may need review for completeness
- The project mixes different naming styles - consider standardizing
