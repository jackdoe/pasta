package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
)

func encrypt(data []byte, key []byte) []byte {
	block, _ := aes.NewCipher(key)
	gcm, err := cipher.NewGCM(block)
	if err != nil {
		log.Fatal(err.Error())
	}
	nonce := make([]byte, gcm.NonceSize())
	if _, err = io.ReadFull(rand.Reader, nonce); err != nil {
		log.Fatal(err.Error())
	}
	ciphertext := gcm.Seal(nonce, nonce, data, nil)
	return ciphertext
}

func decrypt(data []byte, key []byte) []byte {
	block, err := aes.NewCipher(key)
	if err != nil {
		panic(err.Error())
	}
	gcm, err := cipher.NewGCM(block)
	if err != nil {
		log.Fatal(err.Error())
	}
	nonceSize := gcm.NonceSize()
	nonce, ciphertext := data[:nonceSize], data[nonceSize:]
	plaintext, err := gcm.Open(nil, nonce, ciphertext, nil)
	if err != nil {
		log.Fatal(err.Error())
	}
	return plaintext
}

func main() {
	key := make([]byte, 32)
	_, err := rand.Read(key)
	if err != nil {
		log.Fatal(err)
	}

	var clipboard [][]byte
	lock := sync.Mutex{}

	max := 1 // maybe list many for fzf?

	http.HandleFunc("/copy", func(w http.ResponseWriter, r *http.Request) {
		b, _ := ioutil.ReadAll(r.Body)
		b = encrypt(b, key)
		defer r.Body.Close()
		if len(b) > 0 {
			lock.Lock()

			clipboard = append(clipboard, b)
			if len(clipboard) > max {
				clipboard = clipboard[1:]
			}

			lock.Unlock()
		}
	})

	http.HandleFunc("/paste", func(w http.ResponseWriter, r *http.Request) {
		var b []byte

		lock.Lock()
		if len(clipboard) > 0 {
			b = clipboard[len(clipboard)-1]
		}
		lock.Unlock()

		if len(b) > 0 {
			b = decrypt(b, key)
		}
		_, _ = w.Write(b)
	})

	log.Fatal(http.ListenAndServe("127.0.0.1:8111", nil))
}
