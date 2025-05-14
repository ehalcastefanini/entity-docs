<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um componente de interface gráfica baseado em um grid (grade) para exibir e gerenciar informações relacionadas a "Consignee Mill" (provavelmente uma relação entre consignatários e moinhos). Ele permite a configuração de colunas, edição de dados e personalização de propriedades do grid.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Para exibição e manipulação de dados em formato de tabela.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **DBClient:** Para manipulação de dados em memória.
  - **Bibliotecas de Interface Gráfica:** Como `cxStyles`, `cxEditRepositoryItems`, e `sPanel`.

* **Forma:**  
  - **Tipo:** Grid Display.
  - **Colunas do Grid e seus Tipos:**
    - `checked` (Checkbox).
    - `millCode` (Texto).
    - `millDesc` (Texto).
    - `millConsCode` (Texto).
  - **Ações do Grid e seus Efeitos:**
    - Configuração de colunas como editáveis ou somente leitura.
    - Definição de campos ocultos e ordem de exibição.
    - Personalização de editores para colunas específicas.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Configurar colunas do grid como editáveis ou somente leitura.
  - Definir campos ocultos e ordem de exibição.
  - Adicionar editores personalizados para colunas específicas.
  - Obter unidades de negócios selecionadas.

* **Componentes Principais:**
  - `TFRAMEconsMill`: Classe principal que herda de `TFRAMEBaseGridEditSOA`.
  - `cxEDTchecked`: Item de repositório de edição para checkbox.
  - `GridSettings`: Configurações do grid, como campos ocultos, ordem e editores personalizados.

* **Pseudo-código de Ações e Eventos:**
  - `OnCreate` do Frame:  
    ``` 
    ao inicializar o frame:
        configurar propriedades principais
        configurar visibilidade do painel de ações
        definir campos ocultos, ordem e editores personalizados no grid
    ```
  - `SetKeyEditing`:
    ```
    se chave de edição for definida:
        desativar edição da coluna 'millCode'
    ```
  - `SetColumnState`:
    ```
    se coluna for encontrada:
        definir estado de edição da coluna
        aplicar estilo de conteúdo (editável ou somente leitura)
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do frame (`Create`):
     - Configura propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.
     - Define configurações do grid, como campos ocultos, ordem e editores personalizados.
  2. Interação do Usuário:
     - Usuário pode visualizar e interagir com os dados no grid.
     - Ações como edição de colunas ou seleção de checkbox são processadas.
  3. Funções Executadas:
     - `SetColumnState`: Define o estado de edição de uma coluna.
     - `SetKeyEditing`: Configura a edição de chaves.

* **Dados Necessários:**
  - Dados relacionados a "Consignee Mill", como `millCode`, `millDesc`, e `millConsCode`.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação: Configurar coluna como editável ou somente leitura.
    - Pré-condição: A coluna deve existir no grid.
  - Ação: Obter unidades de negócios selecionadas.
    - Pré-condição: Checkbox deve estar marcado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - `checked`: Valor padrão não definido no código.

* **Validações e Condições dos Campos:**
  - Validações específicas não estão definidas no código.

---

## 5. Funções Principais:

* **`Create`:**  
  Configura as propriedades principais do frame e define as configurações do grid.

* **`SetKeyEditing`:**  
  Desativa a edição da coluna `millCode`.

* **`SetColumnState`:**  
  Define o estado de edição de uma coluna específica e aplica estilos de conteúdo.

* **`GetSelectedBusUnits`:**  
  Retorna as unidades de negócios selecionadas no grid.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição e manipulação de dados em formato de tabela.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `DBClient`: Para manipulação de dados em memória.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base herdada para funcionalidades adicionais.

---

## 9. Listagem de Campos e Validações:

* **Campos no Grid:**
  - `checked` (tipo: checkbox, não obrigatório).
  - `millCode` (tipo: texto, não obrigatório).
  - `millDesc` (tipo: texto, não obrigatório).
  - `millConsCode` (tipo: texto, não obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `checked`: Não mapeado explicitamente.
  - `millCode`: Mapeado para `millCode`.
  - `millDesc`: Mapeado para `millDesc`.
  - `millConsCode`: Mapeado para `millConsCode`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável devido à ausência de lógica complexa.

* **Diagrama de Sequência:**  
  Não aplicável devido à ausência de interações com serviços externos.

* **Exemplo de Código:**  
  ```delphi
  var
    Frame: TFRAMEconsMill;
  begin
    Frame := TFRAMEconsMill.Create(Self);
    Frame.SetKeyEditing(True);
  end;
  ```

* **HTML Representando o Grid:**
  ```html
  <table style="width: 100%; border: 1px solid black; border-collapse: collapse;">
    <thead>
      <tr>
        <th style="border: 1px solid black;">Checked</th>
        <th style="border: 1px solid black;">Mill Code</th>
        <th style="border: 1px solid black;">Mill Description</th>
        <th style="border: 1px solid black;">Mill Cons Code</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="border: 1px solid black; text-align: center;"><input type="checkbox" /></td>
        <td style="border: 1px solid black;">001</td>
        <td style="border: 1px solid black;">Mill A</td>
        <td style="border: 1px solid black;">Cons001</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração do Grid:**
  - Definição de campos ocultos, ordem e editores personalizados.
* **Propriedades do Frame:**
  - `MasterKeyFields`, `DataPacketName`, e `FrameType`.

---

## 12. Conclusão:

O código implementa um componente de grid altamente configurável para exibir e gerenciar dados relacionados a "Consignee Mill". Ele é eficiente para personalização de colunas e edição de dados, mas carece de validações explícitas e mensagens de erro.

---

## 13. Resumo Curto:

Componente de grid configurável para exibir e gerenciar dados de "Consignee Mill", com suporte a personalização de colunas, edição e integração com propriedades específicas.#### **FRconsMill.pas**

```
unit FRconsMill;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEconsMill = class(TFRAMEBaseGridEditSOA)
    cxEDTchecked: TcxEditRepositoryCheckBoxItem;
  private
    { Private declarations }
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    function GetSelectedBusUnits: string;
   public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure SetKeyEditing(const EditKey: Boolean); override;

   published
     property SelectedBusUnits: string read GetSelectedBusUnits;

  end;

var
  FRAMEconsMill: TFRAMEconsMill;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, 
  kneTypes;

{$R *.dfm}

{ TFRAMEconsMill }

constructor TFRAMEconsMill.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'consCode=consigneeCode';
  DataPacketName := 'ConsigneeMill';
  PropertyName := 'consigneeMill';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
    
  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('consigneeCode');
    // Ordem Campos ............................................................
    DefineOrderFields('checked; millCode; millDesc; millConsCode');
    // Key Fields ..............................................................
    KeyFields:= 'consigneeCode;millCode';
    // Custom Editors ..........................................................
    AddCustomField('checked','cxEDTchecked');
  end; //with
  ColsWidthInGrid := '80;50;350;100';
  //@@@@@
  CDStable.Tag := 3;
  DStable.Tag := 3;
end;

procedure TFRAMEconsMill.SetKeyEditing(const EditKey: Boolean); 
begin
  inherited;
  SetColumnState('millCode', False);
end; 

// define o estado de uma coluna da grid
procedure TFRAMEconsMill.SetColumnState(pv_ColName: String;
  pv_Edit: Boolean);
var
  lv_column : TcxCustomGridTableItem;
begin
  try
    lv_column := cxDBVtable.GetColumnByFieldName(pv_ColName);
    if Assigned(lv_column) then                     // se encontrou a coluna
    begin                                           // Item normal
      lv_column.Options.Editing := pv_Edit;         // define o estado de edi��o da coluna
      if pv_Edit then
        lv_column.Styles.Content := cxSTLDefault
      else
        lv_column.Styles.Content := cxSTLReadOnly;
```

#### **FRconsMill.dfm**

```
inherited FRAMEconsMill: TFRAMEconsMill
  inherited PNLfooter: TsPanel
    Visible = False
  end
  inherited cxEDTR: TcxEditRepository
    object cxEDTchecked: TcxEditRepositoryCheckBoxItem
      Properties.ValueChecked = '1'
      Properties.ValueUnchecked = '0'
    end
  end
end
```
<!-- tabs:end -->

