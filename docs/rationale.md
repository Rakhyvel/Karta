* `{}` is the only falsey value, to support boolean algebra looking like set algebra
* Maps return `{}` on failed lookup to support sets being maps of their keys to `.t`
* no multimethods, they're too complicated and they can be emulated with map algebra