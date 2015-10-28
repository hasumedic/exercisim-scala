object HelloWorld {
  def hello(greet: String = "World")): String = {
    if(greet == "Sara") "Sara loves Alex!!"
    else s"Hello, $greet!"
  }
}


class HelloWorld {
  public function hello($greet = "World") {
    $response = ($greet == "Sara")
      ? "Sara loves Alex!!"
      : sprintf("Hello %s!", $greet);
    
    return $response;
  }
}