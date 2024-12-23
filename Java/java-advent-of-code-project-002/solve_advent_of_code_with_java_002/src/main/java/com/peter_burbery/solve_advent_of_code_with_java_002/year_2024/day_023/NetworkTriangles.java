package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_023;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class NetworkTriangles {

    public static void main(String[] args) throws IOException {
        solve();
        findLargestClique();
    }

    public static void solve() throws IOException {
        // 1. Read input and build adjacency list
        Map<String, Set<String>> adjacency = new HashMap<>();
        try (BufferedReader reader = new BufferedReader(new FileReader("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-023\\input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                String[] parts = line.split("-");
                String a = parts[0];
                String b = parts[1];

                adjacency.computeIfAbsent(a, k -> new HashSet<>()).add(b);
                adjacency.computeIfAbsent(b, k -> new HashSet<>()).add(a);
            }
        }

        // 2. Find all triangles (sets of 3 computers that are all interconnected)
        Set<Set<String>> triangles = new HashSet<>();

        for (String n : adjacency.keySet()) {
            List<String> neighbors = new ArrayList<>(adjacency.get(n));
            Collections.sort(neighbors);
            for (int i = 0; i < neighbors.size(); i++) {
                for (int j = i + 1; j < neighbors.size(); j++) {
                    String x = neighbors.get(i);
                    String y = neighbors.get(j);
                    if (adjacency.get(x).contains(y)) {
                        Set<String> triangle = new HashSet<>(Arrays.asList(n, x, y));
                        triangles.add(triangle);
                    }
                }
            }
        }

        // 3. Filter those triangles so at least one name starts with 't'
        List<Set<String>> trianglesWithT = triangles.stream()
            .filter(tri -> tri.stream().anyMatch(computer -> computer.startsWith("t")))
            .collect(Collectors.toList());

        // 4. Print results
        System.out.println("Total number of triangles: " + triangles.size());
        System.out.println("Number of triangles containing a computer starting with 't': " + trianglesWithT.size());

        // Uncomment to print all triangles and triangles with 't'
        // System.out.println("All triangles:");
        // for (Set<String> tri : triangles) {
        //     System.out.println(String.join(",", tri));
        // }
        //
        // System.out.println("Triangles with a 't':");
        // for (Set<String> tri : trianglesWithT) {
        //     System.out.println(String.join(",", tri));
        // }
    }

    public static void bronKerbosch(Set<String> R, Set<String> P, Set<String> X, Map<String, Set<String>> adjacency, List<Set<String>> cliques) {
        if (P.isEmpty() && X.isEmpty()) {
            cliques.add(new HashSet<>(R));
        } else {
            String pivot = P.stream().findAny().orElse(null);
            Set<String> nonNeighbors = new HashSet<>(P);
            if (pivot != null) {
                nonNeighbors.removeAll(adjacency.getOrDefault(pivot, Collections.emptySet()));
            }

            for (String v : new HashSet<>(nonNeighbors)) {
                R.add(v);
                Set<String> newP = new HashSet<>(P);
                newP.retainAll(adjacency.getOrDefault(v, Collections.emptySet()));
                Set<String> newX = new HashSet<>(X);
                newX.retainAll(adjacency.getOrDefault(v, Collections.emptySet()));

                bronKerbosch(R, newP, newX, adjacency, cliques);

                R.remove(v);
                P.remove(v);
                X.add(v);
            }
        }
    }

    public static void findLargestClique() throws IOException {
        Map<String, Set<String>> adjacency = new HashMap<>();
        try (BufferedReader reader = new BufferedReader(new FileReader("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-023\\input.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                String[] parts = line.split("-");
                String a = parts[0];
                String b = parts[1];

                adjacency.computeIfAbsent(a, k -> new HashSet<>()).add(b);
                adjacency.computeIfAbsent(b, k -> new HashSet<>()).add(a);
            }
        }

        List<Set<String>> cliques = new ArrayList<>();
        bronKerbosch(new HashSet<>(), new HashSet<>(adjacency.keySet()), new HashSet<>(), adjacency, cliques);

        Set<String> largestClique = cliques.stream()
            .max(Comparator.comparingInt(Set::size))
            .orElse(Collections.emptySet());

        String password = String.join(",", new TreeSet<>(largestClique));
        System.out.println("Password to get into the LAN party: " + password);
    }
}
