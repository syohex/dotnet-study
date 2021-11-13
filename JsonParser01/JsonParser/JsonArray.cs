namespace JsonParser;

public class JsonArray : JsonValue
{
    private List<JsonValue> _value;

    public IEnumerable<JsonValue> Value
    {
        get
        {
            return _value;
        }
    }

    public JsonArray()
    {
        _value = new List<JsonValue>();
        Type = JsonType.Array;
    }
    public JsonArray(List<JsonValue> value)
    {
        _value = value;
        Type = JsonType.Array;
    }

    public void Add(JsonValue value)
    {
        _value.Add(value);
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as JsonArray);
    }

    public bool Equals(JsonArray? other)
    {
        if (other == null)
        {
            return false;
        }

        return _value.SequenceEqual(other._value);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Type, _value, Value);
    }
}