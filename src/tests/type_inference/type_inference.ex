### Testing inference type from operators fuctions
defmodule TypeInferenceTest do
  def sum(a, b) do
    a + b
  end

  def main() do
    sum(1,2)
  end
end
