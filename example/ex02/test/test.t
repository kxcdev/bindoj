Start the server program in the background.
  $ dune exe ../src/server/server.exe > /dev/null &
  $ PORT=8082

Capture it's process id.
  $ PID=`echo $!`

Wait (max 5 sec) for server to start
  $ TIMEOUT=5
  $ URL=localhost:$PORT/_health
  $ ./wait_server_up.sh "$URL"
  # waiting (up to 5 sec) for localhost:8082/_health
  "i am not a tea pot"

Start the client program.
  $ dune exe ../src/client/client.exe
  get_products:
  [ { id = 0;
      details =
      { name = "Pride and Prejudice";
        description =
        "A classic romance novel, describing the love story between Elizabeth Bennet and Fitzwilliam Darcy.";
        price = 800; count = 15 }
      };
    { id = 2;
      details =
      { name = "Dracula";
        description =
        "A Gothic horror novel, telling the story of the vampire Count Dracula.";
        price = 1000; count = 25 }
      };
    { id = 5;
      details =
      { name = "Crime and Punishment";
        description =
        "A novel that explores the moral dilemmas of a young student named Raskolnikov.";
        price = 1500; count = 17 }
      };
    { id = 9;
      details =
      { name = "The Call of the Wild";
        description =
        "A novel about a domesticated dog named Buck and his journey back to the wild.";
        price = 900; count = 20 }
      };
    ]
  register_order:
  Success with order id 10
  get_order:
  { id = 10; total_price = 4200;
    details =
    { products = [(0, 1); (2, 1); (5, 1); (9, 1)];
      payment_method =
      Credit_card {card_number = "1111222233334444";
        holder_name = "John Smith"; expiration_date = (2026, 5); cvv = "123"}
      };
    status = `Unpaid }

Check endpoints
  $ curl -si localhost:$PORT/orders/get \
  >   -d '{"products": [0, 2, 5, 9]}'
  HTTP/1.1 200 Status 200
  Content-Type: application/json
  content-length: 281
  
  [{"id":10,"totalPrice":4200,"details":{"products":[{"_0":0,"_1":1},{"_0":2,"_1":1},{"_0":5,"_1":1},{"_0":9,"_1":1}],"paymentMethod":{"kind":"credit-card","cardNumber":"1111222233334444","holderName":"John Smith","expirationDate":{"_0":2026,"_1":5},"cvv":"123"}},"status":"Unpaid"}]

Check endpoints
  $ curl -si localhost:8082/order/status/update \
  >   -d '{"_0": 10, "_1": "Shipped"}'
  HTTP/1.1 400 Status 400
  Content-Type: application/json
  content-length: 22
  
  "Invalid order status"

Check endpoints
  $ curl -si localhost:8082/product/details/update \
  >   -d '{ "_0": 0, _1: { "price": 1000 } }'
  HTTP/1.1 400 Status 400
  Content-Type: application/json
  content-length: 1547
  
  "Bad request: invalid json format - mandatory field 'name' does not exist at path ._1; expected shape: `with_warning (\n(\"not considering any config if exists\",\n `named ((\"ProductDetailsWithId\",\n          `object_of ([`mandatory_field ((\"_0\",\n                                          `named ((\"ProductId\", `integral))));\n                        `mandatory_field ((\"_1\",\n                                           `named ((\"ProductDetails\",\n                                                    `object_of ([`mandatory_field (\n                                                                  (\"name\",\n                                                                   `string));\n                                                                  `mandatory_field (\n                                                                  (\"description\",\n                                                                   `string));\n                                                                  `mandatory_field (\n                                                                  (\"price\",\n                                                                   `integral));\n                                                                  `mandatory_field (\n                                                                  (\"count\",\n                                                                   `integral))\n                                                                  ])))))\n                        ])))))"

Kill the server process in the background.
  $ kill -9 $PID
