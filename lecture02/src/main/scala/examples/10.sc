// return не используем, лишние скобки можем убрать

def sum(a: Int, b: Int): Int = {
  return a + b
}

def absDelta(a: Int, b: Int): Int = {
  if (a > b) {
    return a - b
  } else {
    return b - a
  }
}

absDelta(3, 4)
absDelta(4, 3)
absDelta(4, 3)
