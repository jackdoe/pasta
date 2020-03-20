package main

import (
	"io"
	"net/http"
	"os"
)

func main() {
	r, err := http.Get("http://localhost:8111/paste")
	if err != nil {
		panic(err)
	}
	defer r.Body.Close()
	_, _ = io.Copy(os.Stdout, r.Body)
}
