#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <algorithm>

// Function to split a string by a delimiter and return a vector of tokens
std::vector<std::string> split(const std::string& str, const std::string& delimiter) {
    std::vector<std::string> tokens;
    size_t prev = 0, pos = 0;
    while ((pos = str.find(delimiter, prev)) != std::string::npos) {
        if (pos > prev)
            tokens.emplace_back(str.substr(prev, pos - prev));
        prev = pos + delimiter.length();
    }
    if (prev < str.length())
        tokens.emplace_back(str.substr(prev, std::string::npos));
    return tokens;
}

// Function to trim whitespace from both ends of a string
std::string trim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\r\n");
    size_t end = s.find_last_not_of(" \t\r\n");
    if (start == std::string::npos)
        return "";
    return s.substr(start, end - start + 1);
}

// Function to check if an update is ordered according to the rules
bool is_update_ordered(const std::vector<int>& update, const std::vector<std::pair<int, int>>& rules) {
    std::unordered_map<int, int> index_map;
    for (size_t i = 0; i < update.size(); ++i) {
        index_map[update[i]] = static_cast<int>(i);
    }

    for (const auto& rule : rules) {
        int x = rule.first;
        int y = rule.second;
        if (index_map.find(x) != index_map.end() && index_map.find(y) != index_map.end()) {
            if (index_map[x] > index_map[y]) {
                return false;
            }
        }
    }
    return true;
}

// Function to perform topological sort on an update according to the rules
std::vector<int> topological_sort_update(const std::vector<int>& update, const std::vector<std::pair<int, int>>& rules) {
    std::unordered_map<int, std::vector<int>> graph;
    std::unordered_map<int, int> in_degree;
    std::unordered_set<int> nodes(update.begin(), update.end());

    // Initialize graph and in-degree
    for (const auto& node : nodes) {
        graph[node] = std::vector<int>();
        in_degree[node] = 0;
    }

    // Build the graph based on rules
    for (const auto& rule : rules) {
        int x = rule.first;
        int y = rule.second;
        if (nodes.find(x) != nodes.end() && nodes.find(y) != nodes.end()) {
            graph[x].push_back(y);
            in_degree[y]++;
        }
    }

    // Initialize queue with nodes having in-degree 0
    std::queue<int> q;
    for (const auto& node : nodes) {
        if (in_degree[node] == 0) {
            q.push(node);
        }
    }

    std::vector<int> sorted_update;
    while (!q.empty()) {
        int current = q.front();
        q.pop();
        sorted_update.push_back(current);

        for (const auto& neighbor : graph[current]) {
            in_degree[neighbor]--;
            if (in_degree[neighbor] == 0) {
                q.push(neighbor);
            }
        }
    }

    // Check if topological sort was possible (i.e., no cycles)
    if (sorted_update.size() != nodes.size()) {
        // Cycle detected or unable to sort
        return std::vector<int>();
    }

    return sorted_update;
}

// Function to get the middle page of an update
int get_middle_page(const std::vector<int>& update) {
    return update[update.size() / 2];
}

int main() {
    std::string file_path = "input.txt";
    std::ifstream infile(file_path);
    if (!infile) {
        std::cerr << "Error: Unable to open the file " << file_path << std::endl;
        return 1;
    }

    // Read the entire file content
    std::stringstream buffer;
    buffer << infile.rdbuf();
    std::string content = trim(buffer.str());

    // Split content into rules and updates based on two consecutive newlines
    std::vector<std::string> sections = split(content, "\n\n");
    if (sections.size() != 2) {
        std::cerr << "Invalid input format. Expected two sections separated by two newlines." << std::endl;
        return 1;
    }

    std::string rules_section = sections[0];
    std::string updates_section = sections[1];

    // Parse rules
    std::vector<std::pair<int, int>> rules;
    std::vector<std::string> rule_lines = split(rules_section, "\n");
    for (const auto& rule_line_raw : rule_lines) {
        std::string rule_line = trim(rule_line_raw);
        if (rule_line.empty())
            continue;
        std::vector<std::string> parts = split(rule_line, "|");
        if (parts.size() != 2) {
            std::cerr << "Invalid rule format: " << rule_line << std::endl;
            continue;
        }
        try {
            int x = std::stoi(trim(parts[0]));
            int y = std::stoi(trim(parts[1]));
            rules.emplace_back(x, y);
        }
        catch (const std::invalid_argument& e) {
            std::cerr << "Invalid number in rule: " << rule_line << std::endl;
        }
        catch (const std::out_of_range& e) {
            std::cerr << "Number out of range in rule: " << rule_line << std::endl;
        }
    }

    // Parse updates
    std::vector<std::vector<int>> updates;
    std::vector<std::string> update_lines = split(updates_section, "\n");
    for (const auto& update_line_raw : update_lines) {
        std::string update_line = trim(update_line_raw);
        if (update_line.empty())
            continue;
        std::vector<std::string> parts = split(update_line, ",");
        std::vector<int> update;
        bool valid = true;
        for (const auto& part_raw : parts) {
            std::string part = trim(part_raw);
            try {
                int page = std::stoi(part);
                update.push_back(page);
            }
            catch (const std::invalid_argument& e) {
                std::cerr << "Invalid number in update: " << update_line << std::endl;
                valid = false;
                break;
            }
            catch (const std::out_of_range& e) {
                std::cerr << "Number out of range in update: " << update_line << std::endl;
                valid = false;
                break;
            }
        }
        if (valid) {
            updates.push_back(update);
        }
    }

    // Identify correctly ordered updates and their middle page numbers
    std::vector<std::vector<int>> correct_updates;
    std::vector<int> middle_pages;

    for (const auto& update : updates) {
        if (is_update_ordered(update, rules)) {
            correct_updates.push_back(update);
            middle_pages.push_back(get_middle_page(update));
        }
    }

    // Calculate the sum of middle pages for correct updates
    long long sum_middle_pages = 0;
    for (const auto& page : middle_pages) {
        sum_middle_pages += page;
    }
    std::cout << "Sum of middle pages for correctly ordered updates: " << sum_middle_pages << std::endl;

    // Identify incorrectly ordered updates, correct them, and collect their middle pages
    std::vector<std::vector<int>> incorrect_updates;
    std::vector<int> incorrect_middle_pages;

    for (const auto& update : updates) {
        if (!is_update_ordered(update, rules)) {
            std::vector<int> corrected_update = topological_sort_update(update, rules);
            if (corrected_update.empty()) {
                std::cerr << "Cycle detected or unable to sort update: ";
                for (size_t i = 0; i < update.size(); ++i) {
                    std::cerr << update[i];
                    if (i != update.size() - 1)
                        std::cerr << ", ";
                }
                std::cerr << std::endl;
                continue;
            }
            incorrect_updates.push_back(corrected_update);
            incorrect_middle_pages.push_back(get_middle_page(corrected_update));
        }
    }

    // Calculate the sum of middle pages for corrected updates
    long long sum_incorrect_middle_pages = 0;
    for (const auto& page : incorrect_middle_pages) {
        sum_incorrect_middle_pages += page;
    }
    std::cout << "Sum of middle pages for corrected updates: " << sum_incorrect_middle_pages << std::endl;

    return 0;
}
