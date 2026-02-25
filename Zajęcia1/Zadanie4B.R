kalkulator = function(a, b, operacja) {
  
  if (operacja == "+") {
    return(a+b)
  } 
  else if (operacja == "-") {
    return (a-b)
  }
  else if (operacja == '*') {
    return (a*b)
  }
  else if (operacja == "/") {
    if (b==0) {
      print("Błąd: dzielenie przez zero!")
    }
    else {
      return (a/b)
    }
  }
  else {
    print("Nieznana operacja")
  }
}