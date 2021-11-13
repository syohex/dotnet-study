using System.Text;

namespace JsonParser;

public class JsonParser
{
    public JsonValue Parse(TextReader reader)
    {
        var input = new JsonInputSource(reader);
        var context = new JsonParseContext();
        return Parse(context, input);
    }

    internal static JsonValue Parse(JsonParseContext context, JsonInputSource input)
    {
        if (!ParseInternal(context, input))
        {
            throw new Exception("failed");
        }

        return context.Value;
    }

    private static bool ParseInternal(JsonParseContext context, JsonInputSource input)
    {
        input.SkipWhiteSpace();

        int ch = input.GetChar();
        switch (ch)
        {
            case 'n':
                if (input.Match("ull") && context.SetNull())
                {
                    return true;
                }
                return false;
            case 't':
                if (input.Match("rue") && context.SetBoolean(true))
                {
                    return true;
                }
                return false;
            case 'f':
                if (input.Match("alse") && context.SetBoolean(false))
                {
                    return true;
                }
                return false;
            case '"':
                string? str = ParseString(input);
                if (str != null)
                {
                    context.SetString(str);
                    return true;
                }
                return false;
            case '[':
                return ParseArray(context, input);
            case '{':
                return ParseObject(context, input);
            default:
                if ((ch >= '0' && ch <= '9') || ch == '-')
                {
                    input.UnGetChar();

                    string numStr = ParseNumber(input);
                    if (String.IsNullOrEmpty(numStr))
                    {
                        return false;
                    }

                    if (Double.TryParse(numStr, out double value))
                    {
                        context.SetNumber(value);
                        return true;
                    }

                    return false;
                }
                break;
        }

        return false;
    }

    private static string ParseNumber(JsonInputSource input)
    {
        var builder = new StringBuilder();
        while (true)
        {
            int ch = input.GetChar();
            if ((ch >= '0' && ch <= '9') || ch == '+' || ch == '-' || ch == 'e' || ch == 'E' || ch == '.')
            {
                builder.Append((char)ch);
            }
            else
            {
                input.UnGetChar();
                break;
            }
        }

        return builder.ToString();
    }

    private static string? ParseString(JsonInputSource input)
    {
        var builder = new StringBuilder();
        while (true)
        {
            int ch = input.GetChar();
            if (ch < ' ') // control character or EOF
            {
                input.UnGetChar();
                return null;
            }

            if (ch == '"')
            {
                return builder.ToString();
            }

            if (ch == '\\')
            {
                ch = input.GetChar();
                if (ch == -1)
                {
                    return null;
                }

                switch (ch)
                {
                    case '"':
                        builder.Append('"');
                        break;
                    case '\\':
                        builder.Append('\\');
                        break;
                    case '/':
                        builder.Append('/');
                        break;
                    case 'b':
                        builder.Append('\b');
                        break;
                    case 'f':
                        builder.Append('\f');
                        break;
                    case 'n':
                        builder.Append('\n');
                        break;
                    case 'r':
                        builder.Append('\r');
                        break;
                    case 't':
                        builder.Append('\t');
                        break;
                    case 'u':
                        string? str = ParseCodePoint(input);
                        if (str == null)
                        {
                            return null;
                        }

                        builder.Append(str);
                        break;
                    default:
                        return null;
                }
            }
            else
            {
                builder.Append((char)ch);
            }
        }

    }
    private static string? ParseCodePoint(JsonInputSource input)
    {
        int codePoint = ParseQuadHex(input);
        if (codePoint == -1)
        {
            return null;
        }

        if (codePoint >= 0xd800 && codePoint <= 0xdfff)
        {
            if (codePoint >= 0xdc00)
            {
                return null;
            }

            if (input.GetChar() != '\\' || input.GetChar() != 'u')
            {
                input.UnGetChar();
                return null;
            }

            int secondCodePoint = ParseQuadHex(input);
            if (!(secondCodePoint >= 0xdc00 && secondCodePoint <= 0xdfff))
            {
                return null;
            }

            codePoint = ((codePoint - 0xd800) << 10) + (secondCodePoint - 0xdc00);
            codePoint += 0x10000;
        }

        return Char.ConvertFromUtf32(codePoint);
    }

    private static int ParseQuadHex(JsonInputSource input)
    {
        int codePoint = 0;
        for (int i = 0; i < 4; i++)
        {
            int hex = input.GetChar();
            if (hex == -1)
            {
                return -1;
            }

            if (hex >= '0' && hex <= '9')
            {
                hex = hex - '0';
            }
            else if (hex >= 'A' && hex <= 'F')
            {
                hex = hex - 'A' + 0xa;
            }
            else if (hex >= 'a' && hex <= 'f')
            {
                hex = hex - 'a' + 0xa;
            }
            else
            {
                input.UnGetChar();
                return -1;
            }

            codePoint = (codePoint << 4) + hex;
        }
        return codePoint;
    }

    private static bool ParseArray(JsonParseContext context, JsonInputSource input)
    {
        if (!context.ParseArrayStart())
        {
            return false;
        }

        if (input.Expect(']'))
        {
            return context.ParseArrayEnd();
        }

        do
        {
            if (!context.ParseArrayItem(input))
            {
                return false;
            }
        } while (input.Expect(','));

        if (input.Expect(']'))
        {
            return context.ParseArrayEnd();
        }

        return true;
    }

    private static bool ParseObject(JsonParseContext context, JsonInputSource input)
    {
        if (!context.ParseObjectStart())
        {
            return false;
        }

        if (input.Expect('}'))
        {
            return context.ParseObjectEnd();
        }

        do
        {
            if (!input.Expect('"'))
            {
                return false;
            }

            string? key = ParseString(input);
            if (key == null)
            {
                return false;
            }

            if (!input.Expect(':'))
            {
                return false;
            }

            if (!context.ParseObjectItem(input, key))
            {
                return false;
            }
        } while (input.Expect(','));

        if (input.Expect('}'))
        {
            return context.ParseObjectEnd();
        }

        return true;
    }
}