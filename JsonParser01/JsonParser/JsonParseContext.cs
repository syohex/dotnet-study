namespace JsonParser;

internal class JsonParseContext
{
    internal JsonValue Value { get; private set; }
    private int _depth;
    private const int DEFAULT_MAX_DEPTH = 100;

    internal JsonParseContext(int depth = DEFAULT_MAX_DEPTH)
    {
        Value = new JsonNull();
        _depth = depth;
    }

    internal bool SetNull()
    {
        Value = new JsonNull();
        return true;
    }

    internal bool SetBoolean(bool value)
    {
        Value = new JsonBoolean(value);
        return true;
    }

    internal bool SetNumber(double value)
    {
        Value = new JsonNumber(value);
        return true;
    }

    internal bool SetString(string value)
    {
        Value = new JsonString(value);
        return true;
    }

    internal bool ParseArrayStart()
    {
        if (_depth == 0)
        {
            return false;
        }

        _depth--;
        Value = new JsonArray();
        return true;
    }

    internal bool ParseArrayItem(JsonInputSource input)
    {
        var context = new JsonParseContext(_depth);
        JsonValue v = JsonParser.Parse(context, input);
        JsonArray array = Value.AsArray();
        array.Add(v);
        return true;
    }

    internal bool ParseArrayEnd()
    {
        _depth++;
        return true;
    }

    internal bool ParseObjectStart()
    {
        if (_depth == 0)
        {
            return false;
        }

        _depth--;
        Value = new JsonObject();
        return true;
    }

    internal bool ParseObjectItem(JsonInputSource input, string key)
    {
        var context = new JsonParseContext(_depth);
        JsonValue v = JsonParser.Parse(context, input);
        JsonObject obj = Value.AsObject();
        obj.Add(key, v);
        return true;
    }

    internal bool ParseObjectEnd()
    {
        _depth++;
        return true;
    }
}