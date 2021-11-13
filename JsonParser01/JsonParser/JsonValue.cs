namespace JsonParser;

public abstract class JsonValue
{
    public JsonType Type { get; protected set; }

    public JsonNull AsNull()
    {
        if (this is not JsonNull)
        {
            throw new Exception("");
        }

        return (JsonNull)this;
    }

    public JsonBoolean AsBoolean()
    {
        if (this is not JsonBoolean)
        {
            throw new Exception("");
        }

        return (JsonBoolean)this;
    }

    public JsonNumber AsNumber()
    {
        if (this is not JsonNumber)
        {
            throw new Exception("");
        }

        return (JsonNumber)this;
    }

    public JsonString AsString()
    {
        if (this is not JsonString)
        {
            throw new Exception("");
        }

        return (JsonString)this;
    }

    public JsonArray AsArray()
    {
        if (this is not JsonArray)
        {
            throw new Exception("");
        }

        return (JsonArray)this;
    }

    public JsonObject AsObject()
    {
        if (this is not JsonObject)
        {
            throw new Exception("");
        }

        return (JsonObject)this;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as JsonValue);
    }

    public bool Equals(JsonValue? other)
    {
        if (other == null)
        {
            return false;
        }
        if (Type != other.Type)
        {
            return false;
        }

        switch (Type)
        {
            case JsonType.Null:
                return true;
            case JsonType.Boolean:
                return AsBoolean().Equals(other.AsBoolean());
            default:
                return false;
        }
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(Type);
    }
}