TEMPLATE := template.ml
BIN := run.out

# Créer un dossier d'exercice avec les fichiers vides
create:
	@echo "📁 Création du dossier $(NAME) et copie de template.ml..."
	@mkdir -p $(NAME)
	@cp template.ml $(NAME)/main.ml
	@echo "(executable (name main))" > $(NAME)/dune
	@echo "(lang dune 3.18)" > $(NAME)/dune-project
	@echo "✍️  Entrez le contenu de $(NAME)/test.input (ligne vide pour terminer) :"
	@rm -f $(NAME)/test.input
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/test.input; \
	done
	@echo "✍️  Entrez le contenu de $(NAME)/test.output (ligne vide pour terminer) :"
	@rm -f $(NAME)/test.output
	@while true; do \
		read -r line; \
		[ -z "$$line" ] && break; \
		echo "$$line" >> $(NAME)/test.output; \
	done
	@echo "✅ Dossier $(NAME) prêt avec test.input et test.output."



# Compiler le programme
build:
	@cd $(NAME) && dune build
	@cp $(NAME)/_build/default/main.exe $(BIN) && chmod u+w $(BIN)

test: build
	@echo "🔍 Test principal..."
	@./run.out $(NAME)/test.input > $(NAME)/test.actual
	@diff -u $(NAME)/test.output $(NAME)/test.actual > $(NAME)/test.diff || true
	@if [ -s $(NAME)/test.diff ]; then \
		echo "❌ Test principal échoué. Différences :"; \
		cat $(NAME)/test.diff; \
	else \
		echo "✅ Test principal réussi."; \
		rm $(NAME)/test.actual $(NAME)/test.diff; \
	fi

	@if [ -d "$(NAME)/tests/inputs" ] && [ -d "$(NAME)/tests/outputs" ]; then \
		echo "🔁 Tests supplémentaires détectés dans $(NAME)/tests/..."; \
		for input_file in $(NAME)/tests/inputs/input_*.txt; do \
			id=$$(basename $$input_file .txt | cut -d'_' -f2); \
			output_file="$(NAME)/tests/outputs/output_$$id.txt"; \
			actual_file="$(NAME)/tests/outputs/actual_$$id.txt"; \
			diff_file="$(NAME)/tests/outputs/diff_$$id.txt"; \
			if [ ! -f $$output_file ]; then \
				echo "⚠️  Fichier de sortie manquant pour test $$id."; \
				continue; \
			fi; \
			./run.out $$input_file > $$actual_file; \
			diff -u $$output_file $$actual_file > $$diff_file || true; \
			if [ -s $$diff_file ]; then \
				echo "❌ Test $$id échoué. Différences :"; \
				cat $$diff_file; \
				exit 1; \
			else \
				echo "✅ Test $$id réussi."; \
				rm -f $$actual_file $$diff_file; \
			fi; \
		done; \
	else \
		echo "ℹ️  Aucun dossier de tests supplémentaires trouvé."; \
	fi

test_all:
	@passed=0; total=0; \
	for d in */ ; do \
		case $$d in \
			.*) ;; \
			*) DIR=$${d%/}; \
			   total=$$((total + 1)); \
			   printf "Testing %s... " "$$DIR"; \
			   $(MAKE) --no-print-directory test NAME=$$DIR > /dev/null 2>&1 && \
			     { echo "Done"; passed=$$((passed + 1)); } || \
			     echo "Fail"; \
			;; \
		esac; \
	done; \
	echo ""; \
	if [ "$$passed" -eq "$$total" ]; then \
		echo "All tests passed ($$passed/$$total)."; \
	else \
		echo "$$passed/$$total tests passed."; \
		exit 1; \
	fi





# Tester + résoudre le vrai dataset
run: build test
	@echo "🚀 Exécution sur le dataset réel..."
	@if [ ! -f $(NAME)/dataset.input ]; then \
		echo "❌ Fichier $(NAME)/dataset.input manquant."; \
		exit 1; \
	fi
	@./run.out $(NAME)/dataset.input > $(NAME)/dataset.output
	@echo "✅ Résultat écrit dans $(NAME)/dataset.output"


# Nettoyage
clean:
	@echo "🧹 Nettoyage des fichiers temporaires..."
	@rm -f $(BIN)
	@rm -f *.fasta
	@find . -type f \( -name "*.cmi" -o -name "*.cmo" -o -name "*.cmx" -o -name "*.o" -o -name "*.annot" \) -delete
	@find . -type f -name "dataset.output" -delete
	@find . -type f -name "dataset.input" -delete


reset:
	@echo "♻️  Réinitialisation de $(NAME)..."
	@rm -f $(NAME)/dataset.output
	@truncate -s 0 $(NAME)/dataset.input
	@echo "✅ $(NAME) a été réinitialisé."

delete:
	@echo "🗑️  Suppression de l'exercice $(NAME)..."
	@if [ -d "$(NAME)" ]; then \
		rm -rf $(NAME); \
		echo "✅ Dossier $(NAME) supprimé."; \
	else \
		echo "⚠️  Aucun dossier nommé $(NAME) trouvé."; \
	fi

