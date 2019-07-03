# Changelog for type-errors

## 0.2.0.0 (2019-07-03)

### Breaking Changes

- The `UnlessPhantom` and various `Subst` type families were broken in the
    presence of stuckness. This rendered them almost entirely unusable --- so
    they now have a new implementation in terms of `te`.
- Added `te`, a TemplateHaskell splice, which will perform symbolic
    substitutions over types. See haddock for the migration details.

## Unreleased changes

