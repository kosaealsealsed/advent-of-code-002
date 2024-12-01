package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_014.part_002;

import pt.kcry.blake3.Blake3;
import scala.math.BigInt;

public class Blake3Example {
	public static void main(String[] args) {
		// Example string input
		String input = "Some string";

		// 1. Standard hashing
		String hashHex = Blake3.newHasher().update(input.getBytes()).doneHex(64);
		System.out.println("Hash (Hex): " + hashHex);

		// 2. Derive key hash
		String deriveKeyHashHex = Blake3.newDeriveKeyHasher("whats the Elvish word for friend").update(input.getBytes())
				.doneHex(64);
		System.out.println("Derived Key Hash (Hex): " + deriveKeyHashHex);

		// 3. Keyed hashing
		String keyedHashHex = Blake3.newKeyedHasher("whats the Elvish word for friend".getBytes())
				.update(input.getBytes()).doneHex(64);
		System.out.println("Keyed Hash (Hex): " + keyedHashHex);

		// 4. Simple hash to hex
		String simpleHashHex = Blake3.hex(input, 64);
		System.out.println("Simple Hash (Hex): " + simpleHashHex);

		// 5. Simple hash to BigInt
		BigInt bigIntHash = Blake3.bigInt(input, BigInt.apply(32));
		System.out.println("Hash (BigInt): " + bigIntHash);

	}
}
