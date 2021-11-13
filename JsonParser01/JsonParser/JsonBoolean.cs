namespace JsonParser;

public class JsonBoolean : JsonValue
{
    public bool Value { get; }
    public JsonBoolean(bool value)
    {
        Type = JsonType.Boolean;
        Value = value;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as JsonBoolean);
    }

    public bool Equals(JsonBoolean? other)
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