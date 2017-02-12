package com.twitter.finagle.netty4

import com.twitter.io.Buf
import io.netty.buffer.Unpooled
import org.junit.runner.RunWith
import org.scalacheck.{Gen, Arbitrary}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{OneInstancePerTest, FunSuite}

@RunWith(classOf[JUnitRunner])
class ByteBufAsBufTest
  extends FunSuite
  with GeneratorDrivenPropertyChecks
  with OneInstancePerTest {

  private val bytes = Array[Byte](1,2,3,4)
  private val underlying = Unpooled.buffer(100)
  underlying.writeBytes(bytes)
  private val buf = new ByteBufAsBuf(underlying)

  test("ByteBufAsBuf.length equals underlying readable bytes") {
    assert(buf.length == 4)
    underlying.readByte()
    assert(buf.length == 3)
  }

  test("writes to underlying ByteBuf are reflected in containing ByteBufAsBuf") {
    assert(Buf.ByteArray.Owned(bytes) == buf)
    val newBytes = Array[Byte](10,20,30,40)
    underlying.writerIndex(0)
    underlying.writeBytes(newBytes)
    assert(Buf.ByteArray.Owned(newBytes) == buf)
  }

  test("writes to slices of the underlying ByteBuf are reflected in ByteBufAsBuf") {
    val bbSlice = underlying.slice(1,2)
    bbSlice.writerIndex(0)
    bbSlice.writeByte(99)
    bbSlice.writeByte(100)

    Buf.ByteArray.Owned.extract(buf).toSeq == Seq(1,99,100,4)
  }

  test("equality") {
    forAll { bytes: Array[Byte] =>
      val baBuf = Buf.ByteArray.Owned(bytes)
      val wrappedBB = new ByteBufAsBuf(Unpooled.wrappedBuffer(bytes))
      val wrappedCopiedBB = new ByteBufAsBuf(Unpooled.copiedBuffer(bytes))
      assert(wrappedBB.equals(baBuf))
      assert(wrappedBB.equals(wrappedCopiedBB))
    }
  }

  test("equality with readerIndex") {
    forAll { bytes: Array[Byte] =>
      whenever(bytes.length > 0) {
        val bb0 = Unpooled.wrappedBuffer(bytes)
        bb0.readByte()

        val bb1 = Unpooled.wrappedBuffer(bytes.drop(1))
        assert(new ByteBufAsBuf(bb0) == new ByteBufAsBuf(bb1))
      }
    }
  }

  test("ByteBufAsBuf.slice") {
    val bufSplits = for {
      b <- Arbitrary.arbitrary[Array[Byte]]
      i <- Gen.choose(0, b.length)
      j <- Gen.choose(i, b.length)
      k <- Gen.choose(j, b.length)
    } yield (b, i, j, k)

    forAll(bufSplits) { case (bytes, i, j, k) =>
      whenever(i <= j && j <= k) {
        val buf = new ByteBufAsBuf(Unpooled.wrappedBuffer(bytes))
        val b1 = buf.slice(i, k)
        val b2 = b1.slice(0, j - i)

        assert(b1.length == k - i)
        assert(b2.length == j - i)
      }
    }
  }

  test("apply(Int)") {
    val out = new Array[Byte](1)
    forAll { bytes: Array[Byte] =>
      whenever(bytes.length >= 2) {
        val byteBuf = Unpooled.wrappedBuffer(bytes)
        byteBuf.readByte()
        val buf = new ByteBufAsBuf(byteBuf)

        // compare slice/write to apply
        buf.slice(0, 1).write(out, 0)
        assert(out(0) == buf(0))

        buf.slice(buf.length-1, buf.length).write(out, 0)
        assert(out(0) == buf(buf.length - 1))
      }
    }
  }

  test("apply(Int) over the length") {
    forAll { bytes: Array[Byte] =>
      val byteBuf = Unpooled.wrappedBuffer(bytes)
      val buf = new ByteBufAsBuf(byteBuf)
      intercept[IndexOutOfBoundsException] {
        buf(bytes.length)
      }
    }
  }

  test("process returns -1 when fully processed") {
    forAll { bytes: Array[Byte] =>
      val buf = new ByteBufAsBuf(Unpooled.wrappedBuffer(bytes))

      var n = 0
      val processor = new Buf.Indexed.Processor {
        def apply(byte: Byte): Boolean = {
          n += 1
          true
        }
      }
      assert(-1 == buf.process(processor))
      assert(buf.length == n)
    }
  }

  test("process returns index where processing stopped") {
    val processor = new Buf.Indexed.Processor {
      def apply(byte: Byte): Boolean = false
    }
    forAll { bytes: Array[Byte] =>
      val buf = new ByteBufAsBuf(Unpooled.wrappedBuffer(bytes))
      assert(buf.process(processor) == (if (buf.isEmpty) -1 else 0))

      def maxThree() = new Buf.Indexed.Processor {
        private[this] var n = 0
        def apply(byte: Byte): Boolean = {
          n += 1
          n <= 3
        }
      }

      if (bytes.length <= 3) {
        assert(-1 == buf.process(maxThree()))
      } else {
        assert(3 == buf.process(maxThree()))
        if (bytes.length > 10) {
          assert(4 == buf.process(1, 5, maxThree()))
          assert(5 == buf.process(2, 9, maxThree()))
          assert(-1 == buf.process(0, 3, maxThree()))
        }
      }
    }
  }

  test("process handles empty inputs") {
    val processor = new Buf.Indexed.Processor {
      def apply(byte: Byte): Boolean = false
    }
    forAll { bytes: Array[Byte] =>
      val buf = new ByteBufAsBuf(Unpooled.wrappedBuffer(bytes))
      val indexed = Buf.Indexed.coerce(buf)
      assert(-1 == indexed.process(1, 0, processor))
      assert(-1 == indexed.process(buf.length, buf.length + 1, processor))
    }
  }

  test("process handles large until") {
    val processor = new Buf.Indexed.Processor {
      def apply(byte: Byte): Boolean = true
    }
    forAll { bytes: Array[Byte] =>
      val buf = new ByteBufAsBuf(Unpooled.wrappedBuffer(bytes))
      val indexed = Buf.Indexed.coerce(buf)
      assert(-1 == indexed.process(0, buf.length, processor))
      assert(-1 == indexed.process(0, buf.length + 1, processor))
    }
  }

  test("process handles readerIndex") {
    val processor = new Buf.Indexed.Processor {
      def apply(byte: Byte): Boolean = false
    }
    forAll { bytes: Array[Byte] =>
      whenever(bytes.length > 1) {
        val bb = Unpooled.wrappedBuffer(bytes)
        bb.readByte()
        val buf = new ByteBufAsBuf(bb)
        val indexed = Buf.Indexed.coerce(buf)
        assert(0 == indexed.process(processor))
        if (buf.length >= 2) {
          assert(1 == indexed.process(1, 2, processor))
        }
      }
    }
  }

}
