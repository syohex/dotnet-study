namespace JsonParser;

public class JsonNull : JsonValue
{
    public JsonNull()
    {
        Type = JsonType.Null;
    }
}