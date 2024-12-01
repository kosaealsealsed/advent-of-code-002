package com.peter_burbery.solve_advent_of_code_with_java_002.year_2015.day_022.part_002;

import java.util.*;
import java.nio.file.*;
public class WizardSimulator {

	static class Player {
		int hp, mana, armor;

		Player(int hp, int mana) {
			this.hp = hp;
			this.mana = mana;
			this.armor = 0;
		}

		Player copy() {
			Player clone = new Player(this.hp, this.mana);
			clone.armor = this.armor;
			return clone;
		}

		void resetArmor() {
			this.armor = 0;
		}
	}

	static class Boss {
		int hp, damage;

		Boss(int hp, int damage) {
			this.hp = hp;
			this.damage = damage;
		}

		Boss copy() {
			return new Boss(this.hp, this.damage);
		}
	}

	static class Spell {
		String name;
		int cost, damage, heal, armor, mana, duration;

		Spell(String name, int cost, int damage, int heal, int armor, int mana, int duration) {
			this.name = name;
			this.cost = cost;
			this.damage = damage;
			this.heal = heal;
			this.armor = armor;
			this.mana = mana;
			this.duration = duration;
		}
	}

	static Map<String, Spell> SPELLS = new HashMap<>();

	static {
		SPELLS.put("Magic Missile", new Spell("Magic Missile", 53, 4, 0, 0, 0, 0));
		SPELLS.put("Drain", new Spell("Drain", 73, 2, 2, 0, 0, 0));
		SPELLS.put("Shield", new Spell("Shield", 113, 0, 0, 7, 0, 6));
		SPELLS.put("Poison", new Spell("Poison", 173, 3, 0, 0, 0, 6));
		SPELLS.put("Recharge", new Spell("Recharge", 229, 0, 0, 0, 101, 5));
	}

	static class GameState implements Comparable<GameState> {
		int manaSpent;
		Player player;
		Boss boss;
		Map<String, Integer> effects;
		boolean isPlayerTurn;

		GameState(int manaSpent, Player player, Boss boss, Map<String, Integer> effects, boolean isPlayerTurn) {
			this.manaSpent = manaSpent;
			this.player = player.copy();
			this.boss = boss.copy();
			this.effects = new HashMap<>(effects);
			this.isPlayerTurn = isPlayerTurn;
		}

		@Override
		public int compareTo(GameState other) {
			return Integer.compare(this.manaSpent, other.manaSpent);
		}
	}

	static void applyEffects(Player player, Boss boss, Map<String, Integer> effects) {
		player.resetArmor();
		for (var entry : effects.entrySet()) {
			String spellName = entry.getKey();
			int timer = entry.getValue();
			if (timer > 0) {
				Spell spell = SPELLS.get(spellName);
				if (spell.armor > 0) {
					player.armor = spell.armor;
				}
				if (spell.damage > 0) {
					boss.hp -= spell.damage;
				}
				if (spell.mana > 0) {
					player.mana += spell.mana;
				}
				effects.put(spellName, timer - 1);
			}
		}
	}

	static int simulateHardMode(Player player, Boss boss) {
		PriorityQueue<GameState> queue = new PriorityQueue<>();
		queue.add(new GameState(0, player, boss, new HashMap<>(), true));
		int minManaSpent = Integer.MAX_VALUE;

		while (!queue.isEmpty()) {
			GameState state = queue.poll();

			// Prune paths that exceed current minimum
			if (state.manaSpent >= minManaSpent)
				continue;

			// Hard mode: player loses 1 HP at the start of their turn
			if (state.isPlayerTurn) {
				state.player.hp -= 1;
				if (state.player.hp <= 0)
					continue; // Player dies immediately
			}

			// Apply effects
			applyEffects(state.player, state.boss, state.effects);

			// Check for win/loss
			if (state.boss.hp <= 0) {
				minManaSpent = Math.min(minManaSpent, state.manaSpent);
				continue;
			}
			if (state.player.hp <= 0) {
				continue;
			}

			if (state.isPlayerTurn) {
				// Player's turn: try each spell
				for (Spell spell : SPELLS.values()) {
					if (state.player.mana < spell.cost)
						continue; // Can't afford
					if (state.effects.containsKey(spell.name) && state.effects.get(spell.name) > 0)
						continue; // Already active

					Player newPlayer = state.player.copy();
					Boss newBoss = state.boss.copy();
					Map<String, Integer> newEffects = new HashMap<>(state.effects);

					newPlayer.mana -= spell.cost;
					int newManaSpent = state.manaSpent + spell.cost;

					if (spell.duration > 0) {
						newEffects.put(spell.name, spell.duration);
					} else {
						newBoss.hp -= spell.damage;
						newPlayer.hp += spell.heal;
					}

					queue.add(new GameState(newManaSpent, newPlayer, newBoss, newEffects, false));
				}
			} else {
				// Boss's turn: attack
				int damage = Math.max(1, state.boss.damage - state.player.armor);
				Player newPlayer = state.player.copy();
				newPlayer.hp -= damage;

				queue.add(new GameState(state.manaSpent, newPlayer, state.boss, state.effects, true));
			}
		}

		return minManaSpent;
	}

	static Boss parseBoss(String input) {
		String[] lines = input.split("\n");
		int hp = Integer.parseInt(lines[0].split(": ")[1]);
		int damage = Integer.parseInt(lines[1].split(": ")[1]);
		return new Boss(hp, damage);
	}

	public static void main(String[] args) {
	    try {
	        // Read input from input.txt
	        String input = Files.readString(Paths.get("Z:\\C\\advent-of-code-002\\jupyter-notebook-python\\2015\\2015_022\\input.txt"));

	        // Parse the boss stats
	        Boss boss = parseBoss(input);

	        // Initialize player
	        Player player = new Player(50, 500);

	        // Find the least mana spent in hard mode
	        int minManaSpentHardMode = simulateHardMode(player, boss);
	        System.out.println("The least amount of mana spent to win in hard mode is: " + minManaSpentHardMode);
	    } catch (Exception e) {
	        e.printStackTrace();
	    }
	}
}
