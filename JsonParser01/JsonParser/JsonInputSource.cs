namespace JsonParser;

internal class JsonInputSource
{
    private TextReader _reader;
    private int _line;
    private int _current;
    private bool _consumed;

    internal JsonInputSource(TextReader reader)
    {
        _reader = reader;
        _line = 1;
        _current = 0;
        _consumed = true;
    }

    internal int GetChar()
    {
        if (_consumed)
        {
            if (_current == '\n')
            {
                _line += 1;
            }

            _current = _reader.Read();
            if (_current == -1) // reach at EOF
            {
                _consumed = false;
                return -1;
            }
        }

        _consumed = true;
        return _current & 0xff;
    }

    internal void UnGetChar()
    {
        _consumed = false;
    }

    internal void SkipWhiteSpace()
    {
        while (true)
        {
            int ch = GetChar();
            if (!(ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'))
            {
                UnGetChar();
                break;
            }
        }
    }

    internal bool Expect(int expected)
    {
        SkipWhiteSpace();

        if (GetChar() != expected)
        {
            UnGetChar();
            return false;
        }

        return true;
    }

    internal bool Match(string pattern)
    {
        foreach (char ch in pattern)
        {
            if (GetChar() != ch)
            {
                UnGetChar();
                return false;
            }
        }

        return true;
    }
}