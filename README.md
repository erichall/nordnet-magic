# nordnet-magic

A fragile attempt to query stock data from your personal account. 

## Usage

Compile: 
``` 
lein uberjar
```

Run:
```
java -jar target/uberjar/nordnet-magic-0.1.0-SNAPSHOT-standalone.jar --config config.edn
```

## Config example
```clojure
{ 
 :investment   1000

 :split        {"Stock A"                                  0.6
                "Stock B"                                  0.3
                "Fund A"                                   0.1}

 ;; keep the overall split or just buy with the investment
 :keep-split   false

 :headless     false

 ;; username-password or bankid
 :login-type   :username

 :login        {:username-base64        "username"
                :password-base64        "pwd"

                :login-username-url     "https://classic.nordnet.se/mux/login/startSE.html?clearEndpoint=0&intent=next"
                :login-bankid-url       "https://www.nordnet.se/se"
                }

 ;; the url with the account you want to get data from
 :account-url  "https://www.nordnet.se/oversikt/konto/2"

 :account-name "the-name"}

```

>>>>>>> fixes
