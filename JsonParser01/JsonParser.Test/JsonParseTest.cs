using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using System.Collections.Generic;

namespace JsonParser.Test;

[TestClass]
public class JsonParseTest
{
    private class TestData<T>
    {
        internal string Input { get; }
        internal T Expected { get; }

        internal TestData(string input, T expected)
        {
            Input = input;
            Expected = expected;
        }

    }

    [TestMethod]
    public void TestParseNull()
    {
        var reader = new StringReader("null");
        var value = new JsonParser().Parse(reader);
        Assert.IsTrue(value.Type == JsonType.Null);
    }

    [TestMethod]
    public void TestParseBoolean()
    {
        var testData = new List<TestData<bool>>()
        {
            new TestData<bool>("true", true),
            new TestData<bool>("false", false),
        };

        foreach (var data in testData)
        {
            var reader = new StringReader(data.Input);
            var value = new JsonParser().Parse(reader);
            Assert.IsTrue(value.Type == JsonType.Boolean);
            Assert.IsTrue(value.AsBoolean().Value == data.Expected);
        }
    }

    [TestMethod]
    public void TestParseNumber()
    {
        var testData = new List<TestData<double>>()
        {
            new TestData<double>("12345", 12345),
            new TestData<double>("0.25", 0.25),
        };

        foreach (var data in testData)
        {
            var reader = new StringReader(data.Input);
            var value = new JsonParser().Parse(reader);
            Assert.IsTrue(value.Type == JsonType.Number);
            Assert.IsTrue(value.AsNumber().Value.CompareTo(data.Expected) == 0);
        }
    }

    [TestMethod]
    public void TestParseString()
    {
        var testData = new List<TestData<string>>()
        {
            new TestData<string>("\"hello\"", "hello"),
            new TestData<string>("\"\\u3042\\u3044\\u3046\\u3048\\u304a\"", "‚ ‚¢‚¤‚¦‚¨"),
        };

        foreach (var data in testData)
        {
            var reader = new StringReader(data.Input);
            var value = new JsonParser().Parse(reader);
            Assert.IsTrue(value.Type == JsonType.String);
            Assert.AreEqual(value.AsString().Value, data.Expected);
        }
    }

    [TestMethod]
    public void TestParseArray()
    {
        var testData = new List<TestData<List<JsonValue>>>()
        {
            new TestData<List<JsonValue>>("[true, 0.5, [null, \"hello\"]]", new List<JsonValue>
            {
                new JsonBoolean(true),
                new JsonNumber(0.5),
                new JsonArray(new List<JsonValue>
                {
                    new JsonNull(),
                    new JsonString("hello"),
                })
            }),
        };

        foreach (var data in testData)
        {
            var reader = new StringReader(data.Input);
            var value = new JsonParser().Parse(reader);
            Assert.IsTrue(value.Type == JsonType.Array);
            Assert.AreEqual(value.AsArray(), new JsonArray(data.Expected));
        }
    }

    [TestMethod]
    public void TestParseObject()
    {
        var testData = new List<TestData<Dictionary<string, JsonValue>>>()
        {
            new TestData<Dictionary<string, JsonValue>>("{\"age\": 10, \"name\": \"tom\", \"hobbies\": [\"guitar\", \"English\"]}", new Dictionary<string, JsonValue>
            {
                {"name", new JsonString("tom")},
                {"age", new JsonNumber(10) },
                {"hobbies", new JsonArray(new List<JsonValue> {new JsonString("guitar"), new JsonString("English")})}
            }),
        };

        foreach (var data in testData)
        {
            var reader = new StringReader(data.Input);
            var value = new JsonParser().Parse(reader);
            Assert.IsTrue(value.Type == JsonType.Object);
            Assert.AreEqual(value.AsObject(), new JsonObject(data.Expected));
        }
    }
}