<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa uma interface de edição e gerenciamento de documentos associados a regras específicas em um sistema. Ele permite que os usuários visualizem, adicionem e excluam documentos relacionados a eventos de uma checklist. O objetivo principal é facilitar a manipulação de documentos vinculados a regras, garantindo que as ações sejam realizadas de forma eficiente e organizada.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes cxGrid:** Para exibição e manipulação de dados em formato de tabela.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **Bibliotecas Personalizadas:** Como `kneFRGridEditSOA`, `kneFGFindUtils`, entre outras, para funcionalidades específicas.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `docCd` (Código do Documento): Tipo string.
      - `docDesc` (Descrição do Documento): Tipo string.
    - **Ações da Grade e seus Efeitos:**
      - Adicionar (`ADD`): Permite adicionar novos documentos à lista.
      - Excluir (`DELETE`): Remove documentos selecionados da lista.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar documentos à lista.
  - Excluir documentos existentes.
  - Configurar a grade para exibir ou ocultar campos específicos.
  - Tornar campos editáveis ou somente leitura.

* **Componentes Principais:**
  - **Grade (Grid):** Exibe os documentos associados às regras.
  - **Painel de Ações:** Permite realizar ações como adicionar ou excluir documentos.
  - **Métodos Privados:** Para manipulação de dados e configuração da interface.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão "Adicionar": `if botão "Adicionar" clicado then execute ACTaddExecute`.
  - Evento `OnClick` do botão "Excluir": `if botão "Excluir" clicado then remova o documento selecionado`.
  - Evento `OnButtonClick` de um campo: `if botão do campo clicado then execute m_FindDocs`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização do componente com o método `Create`, que configura as propriedades principais e a grade.
  - Configuração da grade com o método `GridSetup`, que define campos ocultos, ordem de exibição e editores personalizados.
  - Interação do usuário com a interface, como clicar em botões ou editar campos, que dispara eventos específicos.

* **Dados Necessários:**
  - Código da Regra (`ruleId`): Para identificar a regra associada.
  - Documentos a serem adicionados ou excluídos.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação "Adicionar": Disponível sempre que o painel de ações estiver visível.
  - Ação "Excluir": Disponível apenas quando um documento estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - `mv_AlreadyAdded`: Inicializado como string vazia.

* **Validações e Condições dos Campos:**
  - Campo `docCd`: Configurado como não editável no método `SetKeyEditing`.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Create`: Inicializa o componente e configura propriedades principais.
  - `GridSetup`: Configura a grade, definindo campos ocultos, ordem de exibição e editores personalizados.
  - `SetKeyEditing`: Define se um campo pode ser editado.
  - `ACTaddExecute`: Executa a ação de adicionar um documento.
  - `m_FindDocs`: Manipula a busca de documentos.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código fornecido.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em formato de grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `kneFRGridEditSOA`: Biblioteca personalizada para edição de grades.

* **Componentes Personalizados:**
  - `kneFRGridEditSOA`: Base para o componente de edição de grades.
  - `kneFGFindUtils`: Utilitário para busca de dados.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `docCd` (Tipo: string, não editável).
  - `docDesc` (Tipo: string, editável).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `docCd`: Mapeado para a coluna `docCd` no banco de dados.
  - `docDesc`: Mapeado para a coluna `docDesc` no banco de dados.

---

## 10. Exemplos e Diagramas:

* **Diagramas:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  var
    Frame: TFRAMEdocsCheckRulesDocs;
  begin
    Frame := TFRAMEdocsCheckRulesDocs.Create(Self);
    Frame.ShowActionPanel := True;
    Frame.AvailableActions := 'ADD;DELETE';
  end;
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração da Grade:**
  ```delphi
  DefineHiddenFields('HIDE_ALL_FIELDS');
  DefineOrderFields(mc_GRID_FIELDS);
  AddCustomField('docCd','cxEDTfind');
  ```

* **Configuração de Propriedades:**
  ```delphi
  MasterKeyFields := 'ruleId';
  DataPacketName := 'CheckListDocEvents';
  PropertyName := 'checkListDocEvents';
  ```

---

## 12. Conclusão:

O código implementa uma interface eficiente para gerenciar documentos associados a regras em um sistema. Ele utiliza componentes avançados como cxGrid e bibliotecas personalizadas para oferecer uma experiência de usuário rica. No entanto, faltam mensagens de erro explícitas e validações detalhadas, o que pode ser uma limitação.

---

## 13. Resumo Curto:

O código fornece uma interface para gerenciar documentos vinculados a regras, permitindo adicionar, excluir e configurar exibições em uma grade. Ele utiliza componentes cxGrid e bibliotecas personalizadas para oferecer funcionalidades avançadas e flexíveis.#### **FRdocsCheckRulesDocs.pas**

```
unit FRdocsCheckRulesDocs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel,kneFGFindUtils;

type
  TFRAMEdocsCheckRulesDocs = class(TFRAMEBaseGridEditSOA)
    procedure ACTaddExecute(Sender: TObject);
  private
    mv_AlreadyAdded : string;
    procedure m_FindDocs(Sender: TObject; AButtonIndex: Integer);
    procedure m_AlreadyAdded;
    procedure m_ProcessEachRow(pv_RowIndex: Integer;
      pv_RowInfo: TcxRowInfo);
    procedure GridSetup;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

  end;

var
  FRAMEdocsCheckRulesDocs: TFRAMEdocsCheckRulesDocs;

implementation

uses
  kneTypes, kneFindDialog, kneDialogFactory, kneUtils, kneFGDBUtils, Global
  , CheckListDocsToAddServiceUtils
  ;

const
  mc_GRID_FIELDS = 'docCd;docDesc'; // [12-11-2018] retirado o campo seqNum  

{$R *.dfm}

constructor TFRAMEdocsCheckRulesDocs.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'ruleId';
  DataPacketName := 'CheckListDocEvents';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'checkListDocEvents';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da grelha
  GridSetup;

  mv_AlreadyAdded := '';
end;


procedure TFRAMEdocsCheckRulesDocs.GridSetup;
begin

  with GridSettings do
  begin

    DefineHiddenFields('HIDE_ALL_FIELDS');

    DefineOrderFields(mc_GRID_FIELDS);

//    DefineReadOnlyFields('ALL_FIELDS_READONLY');

    // Custom Editors ..........................................................
    AddCustomField('docCd','cxEDTfind');

  end; //with

//  cxEDTfind.Properties.OnButtonClick := m_FindParams;

end;


procedure TFRAMEdocsCheckRulesDocs.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
	cxDBVtable.GetColumnByFieldName('docCd').Options.Editing := False;
end;


procedure TFRAMEdocsCheckRulesDocs.m_FindDocs(
  Sender: TObject; AButtonIndex: Integer);
```

#### **FRdocsCheckRulesDocs.dfm**

```
dfm file is not needed for this file```
<!-- tabs:end -->

