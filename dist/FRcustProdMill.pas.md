<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica baseado em uma grade (grid) para exibir e gerenciar informações relacionadas a unidades de negócios e moinhos associados a clientes. Ele permite a manipulação de dados, como a seleção de unidades de negócios, e configurações específicas para exibição e edição de colunas. O objetivo principal é fornecer uma interface eficiente para visualizar e editar dados de moinhos de clientes.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes cxGrid:** Utilizados para criar e gerenciar a grade de dados.
  - **SOAP:** Para integração com serviços externos.
  - **Bibliotecas Personalizadas:** Como `kneFRGridEditSOA`, `kneUtils`, e `MillServiceUtils`.

* **Forma:**
  - **Tipo:** Exibição em grade (grid display).
  - **Colunas da Grade e seus Tipos:**
    - `checked` (checkbox).
    - `businessUnit` (string).
    - `millCode` (string).
    - `millDesc` (string).
    - `millCustCode` (string).
    - `sap` (string).
  - **Ações da Grade e seus Efeitos:**
    - Configuração de colunas como somente leitura.
    - Ocultação de colunas.
    - Definição de ordem de exibição das colunas.
    - Adição de editores personalizados para colunas.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Configurar colunas como editáveis ou somente leitura.
  - Ocultar colunas específicas.
  - Definir a ordem de exibição das colunas.
  - Adicionar editores personalizados para colunas específicas.
  - Configurar a visibilidade do painel de ações.

* **Componentes Principais:**
  - `TFRAMEcustProdMill`: Classe principal que gerencia a grade e suas configurações.
  - `cxEDTchecked`: Item de repositório de edição para checkbox.

* **Tradução para Pseudo-código:**
  - Evento `OnCreate`:
    ```pseudo
    ao inicializar o componente:
        configurar campos como somente leitura
        ocultar campos
        definir ordem dos campos
        adicionar editores personalizados
    ```
  - Método `SetColumnState`:
    ```pseudo
    se coluna especificada for encontrada:
        definir estado de edição da coluna
    ```
  - Método `SetForDOCADDR`:
    ```pseudo
    para cada coluna na grade:
        desabilitar edição
    ocultar painel de rodapé
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente (`Create`):
     - Configurações iniciais da grade, como campos somente leitura, campos ocultos, ordem de exibição e editores personalizados.
  2. Interações do Usuário:
     - O usuário pode visualizar e interagir com os dados na grade.
  3. Métodos Específicos:
     - `SetColumnState`: Configura o estado de edição de uma coluna.
     - `SetForDOCADDR`: Desabilita a edição de todas as colunas e oculta o painel de rodapé.

* **Dados Necessários:**
  - Informações sobre unidades de negócios e moinhos, como `businessUnit`, `millCode`, `millDesc`, etc.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação: Configurar colunas como somente leitura.
    - Pré-condição: A coluna deve existir na grade.
  - Ação: Ocultar colunas.
    - Pré-condição: A coluna deve ser especificada.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`Create`:**
  - Configura as propriedades iniciais da grade, como campos somente leitura, campos ocultos, ordem de exibição e editores personalizados.

* **`SetColumnState`:**
  - Define o estado de edição de uma coluna específica.

* **`SetForDOCADDR`:**
  - Desabilita a edição de todas as colunas e oculta o painel de rodapé.

---

## 6. Consumo de Serviços API:

* Não há chamadas explícitas a serviços externos no código fornecido.

---

## 7. Campos Condicionais (Lógica de Formulário):

* Não há campos condicionais definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`: Para exibição e manipulação de grades.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.
  - `kneFRGridEditSOA`: Biblioteca personalizada para edição de grades.

* **Componentes Personalizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base herdada para funcionalidades adicionais.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `checked` (tipo: checkbox, não obrigatório).
  - `businessUnit` (tipo: string, não definido no código).
  - `millCode` (tipo: string, não definido no código).
  - `millDesc` (tipo: string, não definido no código).
  - `millCustCode` (tipo: string, não definido no código).
  - `sap` (tipo: string, não definido no código).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não explicitamente definido no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```delphi
  procedure TFRAMEcustProdMill.SetColumnState(pv_ColName: String; pv_Edit: Boolean);
  begin
    if ColumnExists(pv_ColName) then
      SetColumnEditable(pv_ColName, pv_Edit);
  end;
  ```
* **HTML Renderizado:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <thead>
      <tr>
        <th>Checked</th>
        <th>Business Unit</th>
        <th>Mill Code</th>
        <th>Mill Description</th>
        <th>Mill Customer Code</th>
        <th>SAP</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td><input type="checkbox" /></td>
        <td>Unit 1</td>
        <td>Code 1</td>
        <td>Description 1</td>
        <td>Customer 1</td>
        <td>SAP 1</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração inicial da grade no método `Create`.
* Definição de colunas somente leitura, ocultas e ordem de exibição.

---

## 12. Conclusão:

O código fornece uma interface robusta para exibição e manipulação de dados em uma grade. Ele é altamente configurável, permitindo ajustes como colunas somente leitura, ocultação de colunas e editores personalizados. No entanto, faltam validações explícitas e mensagens de erro, o que pode limitar a usabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O código implementa uma grade configurável para gerenciar dados de moinhos de clientes, permitindo ajustes como colunas somente leitura, ocultação e editores personalizados. Ele é parte de um sistema maior, mas carece de validações e mensagens de erro explícitas.#### **FRcustProdMill.pas**

```
unit FRcustProdMill;

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
  TFRAMEcustProdMill = class(TFRAMEBaseGridEditSOA)
    cxEDTchecked: TcxEditRepositoryCheckBoxItem;
  private
   { Private declarations }
    procedure SetColumnState(pv_ColName: String; pv_Edit: Boolean);
    function GetSelectedBusUnits: string;
   public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    procedure SetKeyEditing(const EditKey: Boolean); override;
    procedure SetForDOCADDR;
    procedure clearCustMillSap;

   published
     property SelectedBusUnits: string read GetSelectedBusUnits;   // [18-03-2016, #22748]
  end;

var
  FRAMEcustProdMill: TFRAMEcustProdMill;

implementation

uses
  kneFindDialogSOA, kneUtils, kneInterfaces, BaseServiceUtils, kneTypes, Global,
  //---
  MillServiceUtils
  , kneFREditSOA;

{$R *.dfm}

{ TFRAMEcustProdMill }

constructor TFRAMEcustProdMill.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode';
  DataPacketName := 'CustomerMill';
  PropertyName := 'customerMill';
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
    DefineReadOnlyFields('businessUnit; millCode; millDesc; millCustCode; sap');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('checked; businessUnit; millCode; millDesc; millCustCode; sap');  // [14-10-2015, #22272]
    // Key Fields ..............................................................
    KeyFields:= 'customerCode;millCode;agentCode';
    // Custom Editors ..........................................................
    AddCustomField('checked','cxEDTchecked');
  end; //with
  ColsWidthInGrid := '75;50;50;350;100';

end;

procedure TFRAMEcustProdMill.SetForDOCADDR;
var
  lv_i : Integer;
begin
  for lv_i := 0 to cxDBVtable.ColumnCount - 1 do
     cxDBVtable.Columns[lv_i].Options.Editing := False;

  PNLfooter.Visible := False;
end;

procedure TFRAMEcustProdMill.SetKeyEditing(const EditKey: Boolean); 
begin
  inherited;
  SetColumnState('millCode', False);
end; 

// define o estado de uma coluna da grid
procedure TFRAMEcustProdMill.SetColumnState(pv_ColName: String;
  pv_Edit: Boolean);
```

#### **FRcustProdMill.dfm**

```
inherited FRAMEcustProdMill: TFRAMEcustProdMill
  ParentFont = True
  inherited PNLfooter: TsPanel
    Visible = False
  end
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
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

