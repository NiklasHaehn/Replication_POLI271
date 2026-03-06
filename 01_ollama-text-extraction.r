library(ollamar)

test_connection()
#pull("llama3.2:latest")
resp <- generate("llama3.2:latest", "tell me a 5-word story")
resp_process(resp, "text")
