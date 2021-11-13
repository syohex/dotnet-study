using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace JsonParser.Test;

[TestClass]
public class JsonValueTest
{
    [TestMethod]
    public void TestNullValue()
    {
        var var1 = new JsonNull();
        var var2 = new JsonNull();

        Assert.IsTrue(var1.Equals(var2));
    }

    [TestMethod]
    public void TestBooleanValue()
    {
        var var1 = new JsonBoolean(true);
        var var2 = new JsonBoolean(false);
        var var3 = new JsonBoolean(true);

        Assert.IsFalse(var1.Equals(var2));
        Assert.IsFalse(var2.Equals(var1));
        Assert.IsTrue(var1.Equals(var3));
        Assert.IsTrue(var3.Equals(var1));
    }

    [TestMethod]
    public void TestNumberValue()
    {
        var var1 = new JsonNumber(1.0);
        var var2 = new JsonNumber(1.0);
        var var3 = new JsonNumber(1.1);

        Assert.IsTrue(var1.Equals(var2));
        Assert.IsFalse(var1.Equals(var3));
    }
}
