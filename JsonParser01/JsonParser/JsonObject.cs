namespace JsonParser;

public class JsonObject : JsonValue
{
    private Dictionary<string, JsonValue> _value;
    public IDictionary<string, JsonValue> Value
    {
        get
        {
            return _value;
        }
    }

    public JsonObject()
    {
        Type = JsonType.Object;
        _value = new Dictionary<string, JsonValue>();
    }

    public JsonObject(Dictionary<string, JsonValue> value)
    {
        Type = JsonType.Object;
        _value = value;
    }

    public void Add(string key, JsonValue value)
    {
        _value.Add(key, value);
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as JsonObject);
    }

    public bool Equals(JsonObject? other)
    {
        if (other == null)
        {
            return false;
        }

        var d1 = new SortedDictionary<string, JsonValue>(_value);
        var d2 = new SortedDictionary<string, JsonValue>(other._value);
        return d1.SequenceEqual(d2);
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(base.GetHashCode(), Type, _value, Value);
    }
}