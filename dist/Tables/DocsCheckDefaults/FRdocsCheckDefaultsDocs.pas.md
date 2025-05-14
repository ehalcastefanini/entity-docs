<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é gerenciar e editar uma lista de documentos padrão em um grid interativo. Ele permite que os usuários adicionem, excluam e editem informações relacionadas a documentos, como código, descrição, status e obrigatoriedade. O problema resolvido é a necessidade de uma interface eficiente para manipular dados de documentos em um formato tabular.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes de interface do usuário como `TcxGrid`, `TcxEditRepository`, e `TcxGridDBTableView`.
  - Serviços SOAP para integração com back-end.
  - Manipulação de dados com `DBClient`.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `docCd` (Código do Documento) - Tipo: String.
      - `required` (Obrigatório) - Tipo: Checkbox (Valores: 'Y' ou 'N').
      - `descrip` (Descrição) - Tipo: String.
      - `stat` (Status) - Tipo: String.
    - **Ações do Grid e seus Efeitos:**
      - Adicionar (`ADD`): Adiciona um novo documento à lista.
      - Excluir (`DELETE`): Remove um documento selecionado da lista.
      - Editar: Permite modificar os valores das colunas editáveis.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um novo documento.
  - Excluir um documento existente.
  - Editar campos específicos de documentos no grid.

* **Componentes Principais:**
  - `TcxGrid`: Exibe os dados em formato tabular.
  - `TcxEditRepositoryCheckBoxItem`: Gerencia o campo de checkbox para a coluna "Obrigatório".
  - `TcxEditRepositoryButtonItem`: Permite a busca de documentos por código.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor de uma célula for alterado então
        validar o campo alterado
    ```
  - Ação `ACTaddExecute`:
    ```pseudo
    se botão "Adicionar" for clicado então
        adicionar um novo documento ao grid
    ```
  - Ação `ACTdeleteExecute`:
    ```pseudo
    se botão "Excluir" for clicado então
        remover o documento selecionado do grid
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização:
     - O componente é carregado com as configurações do grid, incluindo colunas, campos ocultos, e eventos.
  2. Interações do Usuário:
     - O usuário pode adicionar, excluir ou editar documentos no grid.
  3. Eventos:
     - Alterações no grid disparam validações e atualizações.

* **Dados Necessários:**
  - Código do Documento (`docCd`).
  - Obrigatoriedade (`required`).
  - Descrição (`descrip`).
  - Status (`stat`).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um documento estiver selecionado.
  - **Editar:** Apenas campos específicos podem ser editados.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Código inválido" se o código do documento não for encontrado.

* **Valores Padrão dos Campos:**
  - `required`: Valor padrão 'N' (Não obrigatório).

* **Validações e Condições dos Campos:**
  - `docCd`: Deve ser único e não vazio.
  - `required`: Aceita apenas 'Y' ou 'N'.
  - `descrip`: Deve ter no máximo 300 caracteres.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `Create`: Configura o grid e inicializa as propriedades.
  - `SetKeyEditing`: Define os campos que podem ou não ser editados.
  - `m_FindChkLstDocs`: Gerencia a busca de documentos.
  - `ValidateCodDoc`: Valida o código do documento.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `DocCheckListServiceUtils`.
  - Finalidade: Validar e buscar informações de documentos.
  - Dados Enviados: `{ "docCd": "string" }`.
  - Dados Recebidos: `{ "status": "success", "data": "Document object" }`.
  - Tratamento de Erros: Exibe mensagem de erro em caso de falha.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição de dados em formato tabular.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para grids editáveis.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - `docCd` (String, obrigatório, único).
  - `required` (Checkbox, valores: 'Y' ou 'N').
  - `descrip` (String, opcional, máx. 300 caracteres).
  - `stat` (String, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `docCd` → `docCd`.
  - `required` → `required`.
  - `descrip` → `descrip`.
  - `stat` → `stat`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEdocsCheckDefaultsDocs := TFRAMEdocsCheckDefaultsDocs.Create(Self);
  FRAMEdocsCheckDefaultsDocs.AddedDocs := 'Novo Documento';
  ```
* **HTML Renderizado do Grid:**
  ```html
  <table style="width: 100%; border: 1px solid black;">
    <thead>
      <tr>
        <th style="width: 80px;">Código</th>
        <th style="width: 80px;">Obrigatório</th>
        <th style="width: 300px;">Descrição</th>
        <th style="width: 100px;">Status</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>DOC001</td>
        <td>Y</td>
        <td>Documento Exemplo</td>
        <td>Ativo</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial do grid:
  ```delphi
  MasterKeyFields := 'cons;consMkt';
  DataPacketName := 'CheckListDocDef';
  PropertyName := 'chkLstDocDef';
  ```

* Definição de campos ocultos e ordem:
  ```delphi
  HiddenFields.Add('HIDE_ALL_FIELDS');
  DefineOrderFields('docCd;required;descrip;stat');
  ```

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar documentos em um grid interativo. Ele é bem estruturado, com validações e configurações claras. No entanto, faltam filtros avançados e mensagens de erro mais detalhadas.

---

## 13. Resumo Curto:

Este código implementa um grid interativo para gerenciar documentos padrão, permitindo adicionar, excluir e editar informações. Ele utiliza componentes VCL e serviços SOAP para integração com back-end, garantindo uma interface eficiente e funcional.#### **FRdocsCheckDefaultsDocs.pas**

```
unit FRdocsCheckDefaultsDocs;

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
  TFRAMEdocsCheckDefaultsDocs = class(TFRAMEBaseGridEditSOA)
    cxCHKrequired: TcxEditRepositoryCheckBoxItem;

    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure ACTdeleteExecute(Sender: TObject);
  private
    FAddedDocs: String;
    { Private declarations }
    procedure m_FindChkLstDocs(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeChkLstDocs(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure SetAddedDocs(const Value: String);
    procedure ValidateCodDoc;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    property AddedDocs: String read FAddedDocs write SetAddedDocs;

    procedure SetKeyEditing(const EditKey: Boolean); override;
  end;

var
  FRAMEdocsCheckDefaultsDocs: TFRAMEdocsCheckDefaultsDocs;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, Global,
  kneTypes, kneFindDialog, kneDialogFactory, kneConfigObjects,
  //---
  DocCheckListServiceUtils;

{$R *.dfm}

constructor TFRAMEdocsCheckDefaultsDocs.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'cons;consMkt';
  DataPacketName := 'CheckListDocDef';         // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'chkLstDocDef';          // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';
  PNLfooter.Visible := True;

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Hidden ...........................................................
    HiddenFields.Add('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('docCd;required;descrip;stat');
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('descrip;cons;consName;consMkt;marketDescrip');
    // Key Fields ..............................................................
    KeyFields:= 'cons;consMkt;docCd';
    // Custom Editors ..........................................................
    AddCustomField('docCd','cxEDTfind');
    AddCustomField('required','cxCHKrequired');
    AddCustomField('stat','cxEDTstat');

  end; //with

  ColsWidthInGrid := '80;80;300;100';

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindChkLstDocs;
end;

procedure TFRAMEdocsCheckDefaultsDocs.SetKeyEditing(
  const EditKey: Boolean);
begin
  inherited;
  SetNoEdittingInGridFields('docCd;required;stat', self);
end;


```

#### **FRdocsCheckDefaultsDocs.dfm**

```
inherited FRAMEdocsCheckDefaultsDocs: TFRAMEdocsCheckDefaultsDocs
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
      OptionsView.ShowEditButtons = gsebAlways
    end
  end
  inherited cxEDTR: TcxEditRepository
    inherited cxEDTfind: TcxEditRepositoryButtonItem
      Properties.CharCase = ecUpperCase
    end
    object cxCHKrequired: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = 'Y'
      Properties.ValueUnchecked = 'N'
    end
  end
end
```
<!-- tabs:end -->

