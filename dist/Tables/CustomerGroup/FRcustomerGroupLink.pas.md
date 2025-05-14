<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**  
  O código `FRcustomerGroupLink` implementa um componente de interface gráfica baseado em um grid (grade) que exibe e gerencia dados relacionados a grupos de clientes. Ele é projetado para ser usado em sistemas que necessitam de uma interface para visualizar, editar e organizar informações de grupos de clientes vinculados. O componente herda funcionalidades de uma classe base (`TFRAMEBaseGridEditSOA`) e adiciona configurações específicas para o grid, como campos somente leitura, campos ocultos e ordem de exibição.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação principal.
  - **Componentes VCL:** Incluindo `TcxGrid`, `TcxGridDBTableView` e outros componentes visuais para a construção da interface.
  - **SOAP:** Para comunicação com serviços web.
  - **Banco de Dados:** Integração com dados via `DBClient` e `cxDBData`.

* **Forma Identificada:**  
  - **Exibição em Grade (Grid Display):**  
    - **Colunas da Grade e seus Tipos:**  
      - `mill` (string, somente leitura).  
      - `millGroup` (string, somente leitura).  
      - `descrip` (string, somente leitura).  
      - `stat` (string, somente leitura).  
      - `millDtime` (datetime, somente leitura).  
    - **Ações da Grade e seus Efeitos:**  
      - Configuração de campos somente leitura.  
      - Ocultação de campos.  
      - Definição de ordem de exibição das colunas.  

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**  
  - Exibir dados de grupos de clientes em uma grade.  
  - Configurar campos como somente leitura ou ocultos.  
  - Definir a ordem de exibição das colunas.  

* **Componentes Principais:**  
  - `TcxGrid`: Componente principal para exibição de dados em formato de grade.  
  - `TcxGridDBTableView`: Subcomponente que define a visualização dos dados na grade.  
  - `GridSettings`: Configurações específicas da grade, como campos somente leitura, ocultos e ordem de exibição.  

* **Tradução para Pseudo-código:**  
  - `Ao inicializar o componente: configurar campos somente leitura, ocultos e ordem das colunas.`  
  - `Se o usuário interagir com a grade: exibir os dados configurados.`  

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. Inicialização do componente `TFRAMEcustomerGroupLink`.  
  2. Configuração das propriedades da grade, como campos somente leitura, ocultos e ordem de exibição.  
  3. Exibição dos dados na interface gráfica.  

* **Dados Necessários:**  
  - Dados relacionados a grupos de clientes, como `mill`, `millGroup`, `descrip`, `stat` e `millDtime`.  

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - A grade exibe os dados somente após a configuração das propriedades.  
  - Campos somente leitura não podem ser editados pelo usuário.  

* **Filtros Disponíveis:**  
  - Não há filtros explícitos definidos no código.  

* **Mensagens de Erro:**  
  - Não há mensagens de erro explícitas definidas no código.  

* **Valores Padrão dos Campos:**  
  - Não há valores padrão explícitos definidos no código.  

* **Validações e Condições dos Campos:**  
  - Campos definidos como somente leitura não podem ser editados.  
  - Campos ocultos não são exibidos na interface.  

---

## 5. Funções Principais:

* **`Create` (Construtor):**  
  - Configura as propriedades da grade, como campos somente leitura, ocultos e ordem de exibição.  

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
  - `SOAPHTTPClient`: Para comunicação com serviços web.  

* **Componentes Customizados:**  
  - `TFRAMEBaseGridEditSOA`: Classe base herdada para funcionalidades adicionais.  

---

## 9. Listagem de Campos e Validações:

* **Campos na Grade:**  
  - `mill` (string, somente leitura).  
  - `millGroup` (string, somente leitura).  
  - `descrip` (string, somente leitura).  
  - `stat` (string, somente leitura).  
  - `millDtime` (datetime, somente leitura).  

* **Mapeamento de Valores e Colunas do Banco de Dados:**  
  - Não definido explicitamente no código.  

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável devido à simplicidade do código.  

* **Diagrama de Sequência:**  
  Não aplicável devido à simplicidade do código.  

* **Trechos de Código:**  
  ```delphi
  constructor TFRAMEcustomerGroupLink.Create(AOwner: TComponent);
  begin
    inherited;
    MasterKeyFields := 'groupCode=group';
    DataPacketName := 'LnkGroup';
    PropertyName := 'links';
    FrameType := frtDetail;

    with GridSettings do
    begin
      DefineReadOnlyFields('mill; millGroup; descrip; stat; millDtime');
      DefineHiddenFields('HIDE_ALL_FIELDS');
      DefineOrderFields('mill; millGroup; descrip; stat; millDtime');
      KeyFields := 'group';
    end;
  end;
  ```

* **HTML Representando a Grade:**  
  ```html
  <table style="width: 100%; border: 1px solid black; border-collapse: collapse;">
    <thead>
      <tr>
        <th style="border: 1px solid black;">mill</th>
        <th style="border: 1px solid black;">millGroup</th>
        <th style="border: 1px solid black;">descrip</th>
        <th style="border: 1px solid black;">stat</th>
        <th style="border: 1px solid black;">millDtime</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="border: 1px solid black;">Exemplo 1</td>
        <td style="border: 1px solid black;">Grupo 1</td>
        <td style="border: 1px solid black;">Descrição 1</td>
        <td style="border: 1px solid black;">Ativo</td>
        <td style="border: 1px solid black;">2023-10-01</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração de Propriedades da Grade:**  
  ```delphi
  MasterKeyFields := 'groupCode=group';
  DataPacketName := 'LnkGroup';
  PropertyName := 'links';
  FrameType := frtDetail;
  ```

* **Configuração de Campos da Grade:**  
  ```delphi
  DefineReadOnlyFields('mill; millGroup; descrip; stat; millDtime');
  DefineHiddenFields('HIDE_ALL_FIELDS');
  DefineOrderFields('mill; millGroup; descrip; stat; millDtime');
  ```

---

## 12. Conclusão:

O código `FRcustomerGroupLink` é uma implementação eficiente para exibição e gerenciamento de dados em formato de grade. Ele permite configurar campos como somente leitura, ocultos e definir a ordem de exibição. No entanto, faltam validações explícitas, mensagens de erro e filtros, o que pode limitar sua funcionalidade em cenários mais complexos.

---

## 13. Resumo Curto:

O `FRcustomerGroupLink` é um componente de grade para exibição e gerenciamento de dados de grupos de clientes, com suporte para campos somente leitura, ocultos e ordenação. Ele é ideal para sistemas que necessitam de uma interface simples e configurável para visualização de dados.#### **FRcustomerGroupLink.pas**

```
unit FRcustomerGroupLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEcustomerGroupLink = class(TFRAMEBaseGridEditSOA)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustomerGroupLink: TFRAMEcustomerGroupLink;

implementation

uses
  kneTypes, Global;

{$R *.dfm}

{ TFRAMEcustomerGroupLink }

constructor TFRAMEcustomerGroupLink.Create(AOwner: TComponent);
var
  lv_form:  TCustomForm;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'groupCode=group';
  DataPacketName := 'LnkGroup';        // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'links';                  // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := '';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('mill; millGroup; descrip; stat; millDtime');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; millGroup; descrip; stat; millDtime');
    // Key Fields ..............................................................
    KeyFields:= 'group';
    // Custom Editors ..........................................................
//    AddCustomField('marketCode','cxEDTfindMarket');
//    UseColsBestFit := False;
  end; //with

end;


end.
```

#### **FRcustomerGroupLink.dfm**

```
inherited FRAMEcustomerGroupLink: TFRAMEcustomerGroupLink
  inherited cxDBG: TcxGrid
    Enabled = False
    inherited cxDBVtable: TcxGridDBTableView
      Styles.Content = cxSTLReadOnly
    end
  end
end
```
<!-- tabs:end -->

