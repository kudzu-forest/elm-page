# Page

This package allows you to handle various types of pages written in the same way as `Browser.element` or `Browser.sandbox`, unified under a single type without type variables (it's just `Page.Model` and `Page.Msg`, not `Page.Model model msg`). All the `Page.Model`s can be gathered together to form a `List`, `Dict`, `Random.Generator`, or any other data collection you prefer. This makes it much easier to create a single-page application from the source code of a multi-page app.

Be sure to check out the examples in [the GitHub repository](https://github.com/kudzu-forest/elm-page).


# Emitter

This package also provides an `Emitter.Model appMsg` type. The only difference between `Page.Model` and `Emitter appMsg` is that the latter can emit `appMsg` to the main loop of your application. This is useful, for example, for creating well-structured forms with many input fields, managed separately from your main app as an independent module. That example is put in [the GitHub repository](https://github.com/kudzu-forest/elm-page), too.
