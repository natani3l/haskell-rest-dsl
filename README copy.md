# crud-generator-haskell

crud-generator-haskell is a tool for generating CRUD (Create, Read, Update, Delete) APIs for a given data model. It is designed to simplify the process of creating a RESTful API by automating the repetitive tasks involved in generating a CRUD API.

### Defining Models

To use **crud-generator**, you must define data model using a JSON template, like this:
``` json
{
  "name": "user",
  "prefix": "api",
  "entity": "User",
  "fields": [
    {
      "fieldName": "name",
      "fieldType": "text"
    },
    {
      "fieldName": "surname",
      "fieldType": "text"
    }
  ]
}
```

you can define as many models as you project need.

### How to run
To use crud-generator with stack, follow these steps:

  1. First, ensure that you have [Stack](https://docs.haskellstack.org/) installed on your system.

  2. If you dont have [sqlite3](https://www.sqlite.org/download.html) installed, you can download and install.

  3. Open a terminal and navigate to the root directory of your project.

  4. Start the stack ghci REPL by running the following command:


``` sh
  stack ghci
```

In the REPL, run the migrateModel function, passing in the connection string for your database, default is **db.sqlite3**:

``` sh
  migrateModel connectionString
```


This will create the necessary tables in database to support the generated API.

  1. Exit the REPL by typing :quit.

  2. Finally, start the server by running the following command:
#### After running these commands, exit from stack ghci and run the following command to start the server on port 8080:
``` sh
  stack run
```

Or if you prefer, open two terminals side by side, in the first terminal. Run the following command to continuously monitor the source files and trigger a rebuild whenever any file changes:

```sh
  stack build --file-watch
```

So, after this, the project rebuilt whenever a file is modified.

In the second terminal. Run the following command to start the server on port 8080. Where **api** is defined in [package.yaml](package.yaml)
```sh
  stack exec api
```


### Testing the Generated API
#### To test if was working was expect, try to make a request to model that you define, example:
```sh
  curl --location --request GET 'localhost:8080/api/user'
```

And this should return a list of **user**.