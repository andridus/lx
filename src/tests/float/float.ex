defmodule FloatTest do

  def main() do
    2.0 = 1.0 + 1.0
    2.1 = 1.0 + 1.1
    4.4 = 5.5 - 1.1
    2.5 = 5.0 / 2
    9.3 = 3.1 * 3.0

    true = 2.1 > 1.0
    false = 2.2 < 1.1
    true = 2.0 >= 1.0
    false = 2.8 <= 1.1
    :ok
  end
end
