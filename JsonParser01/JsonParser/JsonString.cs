namespace JsonParser;

public class JsonString : JsonValue
{
    public string Value { get; }

    public JsonString(string value)
    {
        Type = JsonType.String;
        Value = value;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as JsonString);
    }

    public bool Equals(JsonString? other)
    {
        if (other == null)
        {
            return false;
        }

        return Value == other.Value;
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Type, Value);
    }
}