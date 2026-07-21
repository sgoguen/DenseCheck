module NatString {
  function digitToString(d: nat): string
    requires d < 10
  {
    match d
    case 0 => "0"
    case 1 => "1"
    case 2 => "2"
    case 3 => "3"
    case 4 => "4"
    case 5 => "5"
    case 6 => "6"
    case 7 => "7"
    case 8 => "8"
    case 9 => "9"
  }

  function natToString(n: nat): string
    decreases n
  {
    if n < 10 then
      digitToString(n)
    else
      natToString(n / 10) + digitToString(n % 10)
  }

  function intToString(n: int): string
  {
    if n < 0 then "-" + natToString((-n) as nat)
    else natToString(n as nat)
  }
}