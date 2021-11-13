namespace JsonParser;

public class JsonNumber : JsonValue
{
    public double Value { get; }

    public JsonNumber(double value)
    {
        Type = JsonType.Number;
        Value = value;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as JsonNumber);
    }

    public bool Equals(JsonNumber? other)
    {
        if (other == null)
        {
            return false;
        }

        return Value.CompareTo(other.Value) == 0;
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Type, Value);
    }
}
