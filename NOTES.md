

crypto stuff - id hash
----------------------

id file

{"1a":"o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg==","1a_secret":"iollyIcHaGeD/JpUNn/7ef1QAzE="}

log from ping using this id

*** public key o0UL/D6qQ+dcSX7hCoyMjLDYeA6dNScZ+YY/fcX4fyCtsSO2u9L5Lg== ***
*** secret key iollyIcHaGeD/JpUNn/7ef1QAzE= ***
loaded hashname 7ecf6a5884d483fde2f6a027e33e6e1756efdb70925557c3e3f776b35329aef5

            HN "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"


From
<https://github.com/telehash/telehash.org/blob/master/hashnames.md>


```json
{
  "2a": "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe",
  "1a": "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a"
}
```

To calculate the hashname the parts are hashed in ascending order by
their CSID, and each part contributes two values, the CSID and the
fingerprint. The hash is rolled up, each resulting binary digest is
combined with the next string value as the input. For the above
example parts, the calculation would look like (in pseudo-code):

```js
hash = sha256("1a")
hash = sha256(hash + "a5c8b5c8a630c84dc01f92d2e5af6aa41801457a")
hash = sha256(hash + "2a")
hash = sha256(hash + "bf6e23c6db99ed2d24b160e89a37c9cd183fb61afeca40c4bc378cf6e488bebe")
print hex(hash)
"825df574f77ebe1380640a314b745ed761d4ec286f0208838bfc14e288b126c0"
```

