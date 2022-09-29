## What is this?
Notes for mosh developers.

## GitHub Actions
In [mosh/build.yml at release_ci · higepon/mosh](https://github.com/higepon/mosh/blob/release_ci/.github/workflows/build.yml) we have
- Build on Ubuntu & macOS.
- clang-tidy
- dist package.

## How to release tar.gz
- Make sure you're on mater branch.
- Make sure [Actions](https://github.com/higepon/mosh/actions) are green.
- Update RELNOTE.md
- Create a tag, push it. It will trigger GitHub Action to create release

```
% git tag mosh-0.2.8 -a -m "mosh-0.2.8"
% git push  origin test-0.0.2
```

