package main

import (
	"net/http"
	"os"
)

func main() {
	_, err := http.Post("http://localhost:8111/copy", "octet/stream", os.Stdin)
	if err != nil {
		panic(err)
	}
}
